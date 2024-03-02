module View.Waveform exposing (Vertex, view)

import Bytes.Encode
import Duration exposing (Duration)
import Effect.WebGL as WebGL exposing (Mesh, Shader)
import Effect.WebGL.Texture as Texture exposing (Texture)
import Html exposing (Html)
import Html.Attributes
import Math.Vector2 exposing (Vec2, vec2)
import Quantity
import Types exposing (Point, RawData)


type alias Vertex =
    { a_position : Vec2 }


type alias Uniforms =
    { u_channel : Texture
    , u_sampleCount : Int
    , u_at : Int
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
        uniform int u_sampleCount;

        void main () {
            float normalized_x = v_position.x / 2. + 0.5;
            vec3 point = texture2D(u_channel, vec2(normalized_x, 0.5)).xyz;
            int pixel_x = int(gl_FragCoord.x - 0.5);
            if (pixel_x == u_at) {
                gl_FragColor = vec4(1., 0., 0., 1.);
            } else {
                if (v_position.y < -point.x || v_position.y > point.z) {
                    discard;
                } else if (abs(v_position.y) > point.y) {
                    gl_FragColor = vec4(0.196, 0.196, 0.784, 1.);
                } else {
                    gl_FragColor = vec4(0.392, 0.392, 0.863, 1.);
                }
            }
        }
    |]


view : Duration -> Maybe Duration -> RawData -> List (Html msg)
view at length channels =
    let
        sampleCount : Int
        sampleCount =
            channels
                |> List.map List.length
                |> List.minimum
                |> Maybe.withDefault 0

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

        webgl : List Point -> Html msg
        webgl channel =
            [ WebGL.entity vertexShader
                fragmentShader
                mesh
                { u_channel = texture channel
                , u_sampleCount = sampleCount
                , u_at =
                    length
                        |> Maybe.map (\l -> round <| toFloat sampleCount * Quantity.ratio at l)
                        |> Maybe.withDefault -1
                }
            ]
                |> WebGL.toHtml
                    [ Html.Attributes.width sampleCount
                    , Html.Attributes.style "width" "100%"
                    , Html.Attributes.height 200
                    , Html.Attributes.style "display" "block"
                    ]
    in
    List.map webgl channels
