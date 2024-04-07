module View.Waveform exposing (Msg(..), Vertex, view)

import Bytes.Encode
import Duration exposing (Duration)
import Effect.WebGL as WebGL exposing (Mesh, Shader)
import Effect.WebGL.Texture as Texture exposing (Texture)
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Math.Vector2 exposing (Vec2, vec2)
import MyUi as Ui exposing (Element, centerX, centerY, el)
import Quantity
import Theme
import Translations
import Types exposing (Point, Selection(..), Track)


type Msg
    = Down Duration
    | Move Duration
    | Up Duration


type alias Vertex =
    { a_position : Vec2 }


type alias Uniforms =
    { u_channel : Texture
    , u_sampleCount : Int
    , u_at : Int
    , u_selection_from : Int
    , u_selection_to : Int
    , u_duration : Float
    , u_offset : Float
    , u_mute : Int
    }


type alias Varyings =
    { v_position : Vec2
    }


tl : Vertex
tl =
    Vertex (vec2 -1 1)


tr : Vertex
tr =
    Vertex (vec2 1 1)


bl : Vertex
bl =
    Vertex (vec2 -1 -1)


br : Vertex
br =
    Vertex (vec2 1 -1)


mesh : Mesh Vertex
mesh =
    WebGL.triangles [ ( tl, tr, bl ), ( tr, br, bl ) ]


unsafeUnwrapResult : Result e a -> a
unsafeUnwrapResult result =
    case result of
        Ok o ->
            o

        Err _ ->
            (\a -> a) (unsafeUnwrapResult result)


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
        attribute vec2 a_position;
        varying vec2 v_position;

        void main () {
            v_position = a_position;
            gl_Position = vec4(a_position, 0., 1.);
        }
    |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec2 v_position;
        uniform sampler2D u_channel;
        uniform int u_at;
        uniform int u_selection_from;
        uniform int u_selection_to;
        uniform float u_offset;
        uniform float u_duration;
        uniform int u_sampleCount;
        uniform int u_mute;

        void main () {
            float normalized_x = (v_position.x / 2. + 0.5 - u_offset) / u_duration;
            vec3 point = texture2D(u_channel, vec2(normalized_x, 0.5)).xyz;
            int pixel_x = int(gl_FragCoord.x - 0.5);
            if (pixel_x == u_at || (pixel_x == u_selection_from && pixel_x == u_selection_to)) {
                gl_FragColor = vec4(0., 0., 0., 1.);
            } else if (normalized_x < 0. || normalized_x > 1.) {
                discard;
            } else if (v_position.y < -point.x || v_position.y > point.z) {
                if (pixel_x >= u_selection_from && pixel_x <= u_selection_to) {
                    gl_FragColor = vec4(0.922, 0.949, 1.,1.);
                } else {
                    gl_FragColor = vec4(0.753, 0.753, 0.753, 1.);
                }
            } else if (u_mute == 1) {
                gl_FragColor = vec4(0.533, 0.533, 0.565, 1.);
            } else if (abs(v_position.y) > point.y) {
                gl_FragColor = vec4(0.196, 0.196, 0.784, 1.);
            } else {
                gl_FragColor = vec4(0.392, 0.392, 0.863, 1.);
            }
        }
    |]


view :
    { at : Duration
    , selection : Maybe { from : Duration, to : Duration }
    , totalLength : Maybe Duration
    }
    -> Track
    -> Element Msg
view config track =
    case track.summary of
        Nothing ->
            el [ centerX, centerY ] <| Theme.text Translations.loadingWaveform

        Just summary ->
            let
                totalLength : Duration
                totalLength =
                    Maybe.withDefault track.duration config.totalLength

                sampleCount : Int
                sampleCount =
                    summary
                        |> List.map List.length
                        |> List.minimum
                        |> Maybe.withDefault 0

                width : Int
                width =
                    sampleCount

                texture : List Point -> Texture
                texture channel =
                    channel
                        |> List.concatMap
                            (\( min, rms, max ) ->
                                [ Bytes.Encode.unsignedInt8 <| round <| 255 * clamp 0 1 -min
                                , Bytes.Encode.unsignedInt8 <| round <| 255 * clamp 0 1 rms
                                , Bytes.Encode.unsignedInt8 <| round <| 255 * clamp 0 1 max
                                ]
                            )
                        |> Bytes.Encode.sequence
                        |> Bytes.Encode.encode
                        |> Texture.loadBytesWith
                            { magnify = Texture.linear
                            , minify = Texture.linear
                            , horizontalWrap = Texture.clampToEdge
                            , verticalWrap = Texture.clampToEdge
                            , flipY = False
                            , premultiplyAlpha = False
                            }
                            ( sampleCount, 1 )
                            Texture.rgb
                        |> unsafeUnwrapResult

                ratio : Duration -> Float
                ratio x =
                    Quantity.ratio x totalLength

                selection : { from : Duration, to : Duration }
                selection =
                    config.selection
                        |> Maybe.withDefault
                            { from = Duration.seconds -1
                            , to = Duration.seconds -1
                            }

                webgl : List Point -> Element Msg
                webgl channel =
                    [ WebGL.entity vertexShader
                        fragmentShader
                        mesh
                        { u_channel = texture channel
                        , u_sampleCount = sampleCount
                        , u_at = round <| toFloat width * ratio config.at
                        , u_selection_from = round <| toFloat width * ratio selection.from
                        , u_selection_to = round <| toFloat width * ratio selection.to
                        , u_offset = ratio track.offset
                        , u_duration = ratio track.duration
                        , u_mute =
                            if track.mute && not track.solo then
                                1

                            else
                                0
                        }
                    ]
                        |> WebGL.toHtml
                            [ Html.Attributes.width width
                            , Html.Attributes.style "width" <| String.fromInt width ++ "px"
                            , Html.Attributes.height 160
                            , Html.Attributes.style "height" "160px"
                            , Html.Attributes.style "display" "block"
                            , Pointer.onDown (toMsg Down width totalLength)
                            , Pointer.onMove (toMsg Move width totalLength)
                            , Pointer.onUp (toMsg Up width totalLength)
                            ]
                        |> Ui.html
            in
            List.map webgl summary
                |> Ui.column [ Ui.width Ui.shrink ]


toMsg : (Duration -> msg) -> Int -> Duration -> Pointer.Event -> msg
toMsg variant width length event =
    let
        ( offsetX, _ ) =
            event.pointer.offsetPos
    in
    variant <| Quantity.multiplyBy (offsetX / toFloat width) length
