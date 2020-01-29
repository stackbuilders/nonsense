module Nonsense exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode
import Random

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }

type Model
  = Error
  | Words (List String)

type Msg
  = GotError
  | GotNoun String
  | GotNounWithArticle String String
  | GotVerb String
  | GotObject String
  | GotObjectWithArticle String String
  | GetAdverb Bool
  | GotAdverb String

init : () -> (Model, Cmd Msg)
init _ =
  (Words [], getWord "nouns" GotNoun)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model) of
    (GotError, _) ->
      (Error, Cmd.none)
    (_, Error) ->
      (Error, Cmd.none)
    (GotNoun noun, Words words) ->
      ( model
      , Random.generate
          (\article -> GotNounWithArticle article noun)
          (articleFor noun)
      )
    (GotNounWithArticle article noun, Words words) ->
      (Words (List.append words [article, noun]), getWord "verbs_past" GotVerb)
    (GotVerb verb, Words words) ->
      (Words (List.append words [verb]), getWord "objects" GotObject)
    (GotObject object, Words words) ->
      ( model
      , Random.generate
          (\article -> GotObjectWithArticle article object)
          (articleFor object)
      )
    (GotObjectWithArticle article object, Words words) ->
      ( Words (List.append words [article, object])
      , Random.generate GetAdverb bool
      )
    (GetAdverb False, _) ->
      (model, Cmd.none)
    (GetAdverb True, _) ->
      (model, getWord "adverbs" GotAdverb)
    (GotAdverb adverb, Words words) ->
      (Words (List.append words [adverb]), Cmd.none)

view : Model -> Html Msg
view model =
  Html.div
    [ Html.Attributes.style "font-family" "sans-serif"
    , Html.Attributes.style "font-size" "xx-large"
    , Html.Attributes.style "position" "fixed"
    , Html.Attributes.style "top" "50%"
    , Html.Attributes.style "left" "50%"
    , Html.Attributes.style "transform" "translate(-50%, -50%)"
    ]
    [ Html.p
      [ Html.Attributes.style "line-height" "1.5"
      ]
      [ case model of
          Error ->
            Html.text "Error!"
          Words [] ->
            Html.text "Loading..."
          Words words ->
            Html.text (String.join " " words)
      ]
    ]

bool : Random.Generator Bool
bool =
  Random.map (\n -> n <= 30) (Random.int 1 100)

isVowel : Char -> Bool
isVowel letter =
  List.member letter ['a', 'e', 'i', 'o', 'u']

articleFor : String -> Random.Generator String
articleFor word =
  let
    check article =
      case (article, String.uncons word) of
        ("a", Just (letter, _)) ->
          if isVowel letter then "an" else "a"
        _ ->
          article
  in
    Random.map check (Random.uniform "a" ["the"])

getWord : String -> (String -> Msg) -> Cmd Msg
getWord set gotWord =
  let
    toMsg result =
      case result of
        Ok (Just word) ->
          gotWord word
        _ ->
          GotError
  in
    Http.get
      { url = "https://api.noopschallenge.com/wordbot?set=" ++ set
      , expect = Http.expectJson toMsg decodeWord
      }

decodeWords : Json.Decode.Decoder (List String)
decodeWords =
  Json.Decode.field "words" (Json.Decode.list Json.Decode.string)

decodeWord : Json.Decode.Decoder (Maybe String)
decodeWord =
  Json.Decode.map List.head decodeWords
