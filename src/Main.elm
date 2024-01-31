module Main exposing (main)

-- Implementation of Boids algorithm in Elm
-- Math based on https://vanhunteradams.com/Pico/Animal_Movement/Boids-algorithm.html
    
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html)
import Random exposing (..)
import List exposing (take)

type alias Dimensions =
    { width : Float, height : Float }

type alias Boid =
    { x : Float,
      y : Float,
      vx : Float,
      vy : Float,
      biasVal: Float}

type alias Context =
    { width : Float,
      height : Float,
      boids : List Boid}

type Msg = Frame Float | NewBoids (List Boid)

type BiasDirection = Left | Right

--- Boid hyperparameters
nBoids : Int
nBoids = 100

protectedRange : Float
protectedRange = 8

avoidFactor : Float
avoidFactor = 0.05

visibleRange : Float
visibleRange = 40

matchingFactor : Float
matchingFactor = 0.05

centeringFactor : Float
centeringFactor = 0.0001

maxSpeed : Float
maxSpeed = 8

minSpeed : Float
minSpeed = 1

turnFactor : Float
turnFactor = 0.1

maxBias : Float
maxBias = 0.05

biasIncrement : Float
biasIncrement = 0.004

main : Program Dimensions Context Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> onAnimationFrameDelta Frame
        }

init : {width : Float, height : Float} -> (Context, Cmd Msg)
init {width, height} =
    ({ width = width
     , height = height
     , boids = []
     }, Random.generate NewBoids (generateBoids (round height) (round width) nBoids))

generateBoids : Int -> Int -> Int -> Generator (List Boid)
generateBoids h w n =
    Random.list n (generateBoid h w)

generateBoid : Int -> Int -> Generator Boid
generateBoid h w =
    let
        x = Random.float 0 (toFloat w)
        y = Random.float 0 (toFloat h)
        vx = Random.float -1 1
        vy = Random.float -1 1
        bias = Random.float 0 0
    in
    map5 Boid x y vx vy bias

view : Context -> Html Msg
view { width, height, boids} =
    Canvas.toHtml
        ( round width, round height )
        []
        ((shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ])::(List.map drawBoid boids))


update : Msg -> Context -> (Context, Cmd Msg)
update msg context =
    case msg of
        Frame _ ->
            let
                newBoids = updateBoids (context.height / 2) (context.width / 2) context.boids
            in
            ( { context | boids = newBoids }, Cmd.none )
        NewBoids newBoids ->
            ( { context | boids = newBoids }, Cmd.none )
drawBoid : Boid -> Renderable
drawBoid boid =
    let
        angle = atan2 boid.vy boid.vx
    in
    shapes
        [ transform
            [ translate boid.x boid.y
            , rotate (degrees angle)
            ]
        , fill (Color.hsl 0.7 0.7 0.7)
        ]
        [ circle (boid.x, boid.y) 5]
    
--- Code to update Boid positions
distance : Boid -> Boid -> Float
distance boid1 boid2 =
    sqrt ((boid1.x - boid2.x) ^ 2 + (boid1.y - boid2.y) ^ 2)


updateSeperation : List Boid -> Boid -> Boid
updateSeperation boids boid =
    let
        (closevx, closevy) = List.foldl (updateSeperationP boid) (0, 0) boids
    in
    { boid | x = boid.x + avoidFactor*closevx, y = boid.y + avoidFactor*closevy}

updateSeperationP : Boid -> Boid -> (Float, Float) -> (Float, Float)
updateSeperationP boid boid2 (closevx, closevy) =
    if distance boid boid2 < protectedRange then
        (closevx + boid.x - boid2.x, closevy + boid.y - boid2.y)
    else
        (closevx, closevy)


updateAlignment : List Boid -> Boid -> Boid
updateAlignment boids boid =
    let
        (n, avgvx_, avgvy_) = List.foldl (updateAlignmentP boid) (0, 0, 0) boids
        avgvx = avgvx_ / toFloat n
        avgvy = avgvy_ / toFloat n
    in
    if n == 0 then
        boid
    else
        { boid | vx = boid.vx + matchingFactor*(avgvx - boid.vx), vy = boid.vy + matchingFactor*(avgvy - boid.vy)}

updateAlignmentP : Boid -> Boid -> (Int, Float, Float) -> (Int, Float, Float)
updateAlignmentP boid boid2 (nNeighbors, avgvx, avgvy) =
    if distance boid boid2 < visibleRange then
        (nNeighbors + 1, avgvx + boid2.vx, avgvy + boid2.vy)
    else
        (nNeighbors, avgvx, avgvy)


updateCohesion : List Boid -> Boid -> Boid
updateCohesion boids boid =
    let
        (n, avgx_, avgy_) = List.foldl (updateCohesionP boid) (0, 0, 0) boids
        avgx = avgx_ / toFloat n
        avgy = avgy_ / toFloat n
    in
    if n == 0 then
        boid
    else
        { boid | vx = boid.vx + centeringFactor*(avgx - boid.x), vy = boid.vy + centeringFactor*(avgy - boid.y)}

updateCohesionP : Boid -> Boid -> (Int, Float, Float) -> (Int, Float, Float)
updateCohesionP boid boid2 (nNeighbors, avgx, avgy) =
    if distance boid boid2 < visibleRange then
        (nNeighbors +1, avgx + boid2.x, avgy + boid2.y)
    else
        (nNeighbors, avgx, avgy)


updateTurn : Boid -> Float -> Float -> Boid
updateTurn boid windowHeight windowWidth =
    let  -- Keep 10% margin on all sides
        leftmargin = 0.1*windowWidth
        rightmargin = 0.9*windowWidth
        topmargin = 0.1*windowHeight
        bottommargin = 0.9*windowHeight
        vx = if boid.x < leftmargin then
                    boid.vx + turnFactor
                else if boid.x > rightmargin then
                    boid.vx - turnFactor
                else
                    boid.vx
        vy = if boid.y < topmargin then
                    boid.vy + turnFactor
                else if boid.y > bottommargin then
                    boid.vy - turnFactor
                else
                    boid.vy
    in
    { boid | vx = vx, vy = vy}


updateSpeed : Boid -> Boid
updateSpeed boid = 
    let
        speed = sqrt (boid.vx ^ 2 + boid.vy ^ 2)
    in
    if speed > maxSpeed then
        let
            vx = (boid.vx / speed) * maxSpeed
            vy = (boid.vy / speed) * maxSpeed
        in
        { boid | vx = vx, vy = vy}
    else if speed < minSpeed then
        let
            vx = (boid.vx / speed) * minSpeed
            vy = (boid.vy / speed) * minSpeed
        in
        { boid | vx = vx, vy = vy}
    else
        boid

-- Bias the boid towards the center of the screen if it is far away
updateBias : BiasDirection -> Boid -> Boid
updateBias direction boid =
    let
        biasVal1 = min (boid.biasVal + biasIncrement) maxBias
        biasVal2 = max biasIncrement (boid.biasVal - biasIncrement)
    in
        case direction of
            Left ->
                if boid.vx < 0 then
                    { boid | biasVal = biasVal1, vx = (1-biasVal1)*boid.vx + biasVal1}
                else
                    { boid | biasVal = biasVal1, vx = (1-biasVal2)*boid.vx + biasVal2}
            Right ->
                if boid.vx < 0 then
                    { boid | biasVal = biasVal1, vx = (1-biasVal1)*boid.vx - biasVal1}
                else
                    { boid | biasVal = biasVal1, vx = (1-biasVal2)*boid.vx - biasVal2}

updatePosition : Boid -> Boid
updatePosition boid =
    { boid | x = boid.x + boid.vx, y = boid.y + boid.vy}


updateBoids : Float -> Float -> List Boid -> List Boid
updateBoids windowHeight windowWidth boids =
    let
        boidsFirst = List.map (updateBias Left) (take 10 boids)
        boidsLast = List.reverse (List.map (updateBias Right) (take 10 (List.reverse boids)))
        boidsMiddle = List.drop 10 (List.take ((List.length boids) - 10) boids)
        boidsFinal = boidsFirst ++ boidsMiddle ++ boidsLast
        boids1 = List.map (updateBoid windowHeight windowWidth boids) boidsFinal
    in
    boids1


updateBoid : Float -> Float -> List Boid -> Boid -> Boid
updateBoid windowHeight windowWidth boids boid =
    let
        boid1 = updateSeperation boids boid
        boid2 = updateAlignment boids boid1
        boid3 = updateCohesion boids boid2
        boid4 = updateTurn boid3 windowHeight windowWidth
        boid5 = updateSpeed boid4
        boid8 = updatePosition boid5
    in
    boid8




