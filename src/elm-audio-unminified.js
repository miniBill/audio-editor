/**
 * @param {{ ports: { audioPortFromJS: { send: (arg0: { type: number; samplesPerSecond?: number; requestId?: number; error?: string; bufferId?: number; durationInSeconds?: number; }) => void; }; audioPortToJS: { subscribe: (arg0: (message: any) => void) => void; }; }; }} app
 */
function startAudio(app) {
  window.AudioContext =
    // @ts-ignore
    window.AudioContext || window.webkitAudioContext || false;
  if (window.AudioContext) {
    /** @type {AudioBuffer[]} */
    let audioBuffers = [];
    let context = new AudioContext();
    /** @type {{ [key: number]: { bufferId: any; nodes: {sourceNode: AudioBufferSourceNode; gainNode: GainNode; volumeAtGainNodes: GainNode[] } } | null }} */
    let audioPlaying = {};

    app.ports.audioPortFromJS.send({
      type: 2,
      samplesPerSecond: context.sampleRate,
    });

    /**
     * @param {string} audioUrl
     * @param {number} requestId
     */
    async function loadAudio(audioUrl, requestId) {
      try {
        const response = await fetch(audioUrl);
        const arrayBuffer = await response.arrayBuffer();
        const buffer = await context.decodeAudioData(arrayBuffer);
        let bufferId = audioBuffers.length;

        // TODO: Read the header of the ArrayBuffer before decoding to an AudioBuffer https://www.mp3-tech.org/programmer/frame_header.html
        // need to use DataViews to read from the ArrayBuffer
        audioBuffers.push(buffer);

        app.ports.audioPortFromJS.send({
          type: 1,
          requestId: requestId,
          bufferId: bufferId,
          durationInSeconds: buffer.length / buffer.sampleRate,
        });
      } catch (error) {
        app.ports.audioPortFromJS.send({
          type: 0,
          requestId: requestId,
          // @ts-ignore
          error: error.message,
        });
      }
    }

    /**
     * @param {number} posix
     * @param {number} currentTimePosix
     */
    function posixToContextTime(posix, currentTimePosix) {
      return (posix - currentTimePosix) / 1000 + context.currentTime;
    }

    /**
     * @param {AudioBufferSourceNode} sourceNode
     * @param {{ loopStart: number; loopEnd: number; } | null} loop
     */
    function setLoop(sourceNode, loop) {
      if (loop) {
        sourceNode.loopStart = loop.loopStart / 1000;
        sourceNode.loopEnd = loop.loopEnd / 1000;
        sourceNode.loop = true;
      } else {
        sourceNode.loop = false;
      }
    }

    /**
     * @param {number} startAt
     * @param {number} startValue
     * @param {number} endAt
     * @param {number} endValue
     * @param {number} time
     */
    function interpolate(startAt, startValue, endAt, endValue, time) {
      let t = (time - startAt) / (endAt - startAt);
      if (Number.isFinite(t)) {
        return t * (endValue - startValue) + startValue;
      } else {
        return startValue;
      }
    }

    /**
     * @param {{ volume: number; time: number; }[][]} volumeAt
     * @param {number} currentTime
     */
    function createVolumeTimelineGainNodes(volumeAt, currentTime) {
      return volumeAt.map((volumeTimeline) => {
        let gainNode = context.createGain();

        gainNode.gain.setValueAtTime(volumeTimeline[0].volume, 0);
        gainNode.gain.linearRampToValueAtTime(volumeTimeline[0].volume, 0);
        let currentTime_ = posixToContextTime(currentTime, currentTime);

        for (let j = 1; j < volumeTimeline.length; j++) {
          let previous = volumeTimeline[j - 1];
          let previousTime = posixToContextTime(previous.time, currentTime);
          let next = volumeTimeline[j];
          let nextTime = posixToContextTime(next.time, currentTime);

          if (nextTime > currentTime_ && currentTime_ >= previousTime) {
            let currentVolume = interpolate(
              previousTime,
              previous.volume,
              nextTime,
              next.volume,
              currentTime_
            );
            gainNode.gain.setValueAtTime(currentVolume, 0);
            gainNode.gain.linearRampToValueAtTime(next.volume, nextTime);
          } else if (nextTime > currentTime_) {
            gainNode.gain.linearRampToValueAtTime(next.volume, nextTime);
          } else {
            gainNode.gain.setValueAtTime(next.volume, 0);
          }
        }

        return gainNode;
      });
    }

    /**
     * @param {AudioNode[]} nodes
     */
    function connectNodes(nodes) {
      for (let j = 1; j < nodes.length; j++) {
        nodes[j - 1].connect(nodes[j]);
      }
    }

    /**
     * @param {AudioBuffer} buffer
     * @param {number} volume
     * @param {{ volume: number; time: number; }[][]} volumeTimelines
     * @param {number} startTime
     * @param {number} startAt
     * @param {number} currentTime
     * @param {{ loopEnd: number; loopStart: number; } | null} loop
     * @param {number} playbackRate
     * @returns {{ sourceNode: AudioBufferSourceNode; gainNode: GainNode; volumeAtGainNodes: GainNode[] }}
     */
    function playSound(
      buffer,
      volume,
      volumeTimelines,
      startTime,
      startAt,
      currentTime,
      loop,
      playbackRate
    ) {
      let mp3MarginInSeconds = 0;
      let source = context.createBufferSource();

      if (loop) {
        // Add an extra 10 seconds so there's some room if the loopEnd gets moved back later
        let durationInSeconds =
          10 + loop.loopEnd / 1000 - buffer.length / buffer.sampleRate;
        if (durationInSeconds > 0) {
          let sampleCount =
            buffer.getChannelData(0).length +
            Math.ceil(durationInSeconds * buffer.sampleRate);
          let newBuffer = context.createBuffer(
            buffer.numberOfChannels,
            sampleCount,
            context.sampleRate
          );

          for (let i = 0; i < buffer.numberOfChannels; i++) {
            newBuffer.copyToChannel(buffer.getChannelData(i), i);
          }
          source.buffer = newBuffer;
        } else {
          source.buffer = buffer;
        }
      } else {
        source.buffer = buffer;
      }

      source.playbackRate.value = playbackRate;
      setLoop(source, loop);

      let timelineGainNodes = createVolumeTimelineGainNodes(
        volumeTimelines,
        currentTime
      );

      let gainNode = context.createGain();
      gainNode.gain.setValueAtTime(volume, 0);

      connectNodes([
        source,
        gainNode,
        ...timelineGainNodes,
        context.destination,
      ]);

      if (startTime >= currentTime) {
        source.start(
          posixToContextTime(startTime, currentTime),
          mp3MarginInSeconds + startAt / 1000
        );
      } else {
        // TODO: offset should account for looping
        let offset = (currentTime - startTime) / 1000;
        source.start(0, offset + mp3MarginInSeconds + startAt / 1000);
      }

      return {
        sourceNode: source,
        gainNode: gainNode,
        volumeAtGainNodes: timelineGainNodes,
      };
    }

    app.ports.audioPortToJS.subscribe((message) => {
      let currentTime = new Date().getTime();
      for (let i = 0; i < message.audio.length; i++) {
        let audio =
          /** @type {{nodeGroupId:number, action:string}} */ message.audio[i];
        switch (audio.action) {
          case "stopSound": {
            let value = audioPlaying[audio.nodeGroupId];
            audioPlaying[audio.nodeGroupId] = null;
            if (!value) break;
            value.nodes.sourceNode.stop();
            value.nodes.sourceNode.disconnect();
            value.nodes.gainNode.disconnect();
            value.nodes.volumeAtGainNodes.map((node) => node.disconnect());
            break;
          }
          case "setVolume": {
            let value = audioPlaying[audio.nodeGroupId];
            if (!value) break;
            value.nodes.gainNode.gain.setValueAtTime(audio.volume, 0);
            break;
          }
          case "setVolumeAt": {
            let value = audioPlaying[audio.nodeGroupId];
            if (!value) break;
            value.nodes.volumeAtGainNodes.map((node) => node.disconnect());
            value.nodes.gainNode.disconnect();

            let newGainNodes = createVolumeTimelineGainNodes(
              audio.volumeAt,
              currentTime
            );

            connectNodes([
              value.nodes.gainNode,
              ...newGainNodes,
              context.destination,
            ]);

            value.nodes.volumeAtGainNodes = newGainNodes;
            break;
          }
          case "setLoopConfig": {
            let value = audioPlaying[audio.nodeGroupId];
            if (!value) break;

            /* TODO: Resizing the buffer if the loopEnd value is past the end of the buffer.
              This might not be possible to do so the alternative is to create a new audio
              node (this will probably cause a popping sound and audio that is slightly out of sync).
            */

            setLoop(value.nodes.sourceNode, audio.loop);
            break;
          }
          case "setPlaybackRate": {
            let value = audioPlaying[audio.nodeGroupId];
            if (!value) break;
            value.nodes.sourceNode.playbackRate.setValueAtTime(
              audio.playbackRate,
              0
            );
            break;
          }
          case "startSound": {
            let nodes = playSound(
              audioBuffers[audio.bufferId],
              audio.volume,
              audio.volumeTimelines,
              audio.startTime,
              audio.startAt,
              currentTime,
              audio.loop,
              audio.playbackRate
            );
            audioPlaying[audio.nodeGroupId] = {
              bufferId: audio.bufferId,
              nodes: nodes,
            };
            break;
          }
        }
      }

      for (let i = 0; i < message.audioCmds.length; i++) {
        loadAudio(
          message.audioCmds[i].audioUrl,
          message.audioCmds[i].requestId
        );
      }
    });
  } else {
    console.log("Web audio is not supported in your browser.");
  }
}
