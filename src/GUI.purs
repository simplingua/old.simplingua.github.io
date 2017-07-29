module GUI
(mainGui)
where

import Prelude (Unit, bind, void, ($), (<$>), (<<<), (<>), (>>=))


import Control.Monad.Eff (Eff)

import Data.String (charAt)
import ReactDOM (render) as R
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (Maybe(..))
import Data.Array (singleton, take)
import Thermite (PerformAction, Render, Spec, createClass, simpleSpec, writeState)
import Data.Foldable (traverse_)

import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToNonElementParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.NonElementParentNode (getElementById) as DOM
import DOM.Node.Types (ElementId(ElementId)) as DOM
import React (createFactory, ReactElement) as R
import React.DOM (a, blockquote, div, footer, form, h5, header', input, label, li, main', nav', p, text, ul) as R
import React.DOM.Props as RP

import Dictionary (Dictionary(..), queryDict)

type MyState = Array Dictionary

data MyAction = Query String

render :: forall props. Render MyState props MyAction
render dispatch _ dicts _ =
  let
    container = R.div [RP.className "container"]
    inputArea = R.form [RP.property "onSubmit=\"return false;\""] <<<
      singleton $ R.div [RP.className "input-field"] $
        [ R.input [RP.className "center-align validate", RP._id "search", RP.typeof "search", RP.required true, RP.placeholder "Lexo de serka", RP.onInput \e -> dispatch (Query $ (unsafeCoerce e).target.value)] []
        , R.label [RP.property "for=\"search\""] [R.text "SIMPLINGO"]
        ]
    resultList = R.div [RP.className "col s4"] <<< singleton <<< renderDictionary <$> dicts
    addHeader = R.header' <<< singleton
    navi = R.nav' <<< singleton <<< R.div [RP.className "nav-wrapper"]  <<< singleton -- R.div [RP.className "row"] [R.div [RP.className "col s3"] [l], R.div [RP.className "col s9"] [i]]
    mainBody = R.main' <<< singleton $ container $ [R.div [RP.className "row"] resultList]
    links = linkList  "grammar.pdf" "pronunciation.pdf" "simplingua.xlsx"
  in
    [navi inputArea, mainBody, myFooter links]

renderDictionary :: Dictionary -> R.ReactElement
renderDictionary (Dictionary dic) =
  let
    r = case dic.root of
      Nothing -> []
      Just rr -> [ R.div [ RP.className "card-action" ]
                     [ R.a [ RP.className "" ] [R.text rr]
                     ]
                 ]

    reColor = case charAt 0 dic.spell of
      Just '/' -> "grey darken-1"
      _ -> "grey darken-1"-- "blue-grey"

    ch = R.div [RP.className $ "btn " <> colorPerfer dic.chara] [R.text dic.chara]

    lv = case dic.level of
      Nothing -> []
      Just l -> singleton $ R.div [RP.className $ "btn-floating halfway-fab center-align " <> levelColor l] [R.text l]

  in
    -- [R.p [] [R.text dic.spell] , R.p' $ [R.q' [R.text dic.meanning]] <> r]
    R.blockquote [ RP.className "red lighten-2 center-aligh" ]
      [ R.div [ RP.className $ "card " <> reColor <> " hoverable" ] $
          [ R.div [ RP.className "card-content" ]
              [ R.div [ RP.className "row" ] [ R.div [RP.className "flow-text white-text col s5"] [R.text dic.spell], R.div [RP.className "right !important"] [ch]]
              , R.div [ RP.className "" ] <<< singleton $ renderMeanings dic.meanings
              ]
          , R.div [ RP.className "card-image" ] lv
          ] <> r
      ]

colorPerfer :: String -> String
colorPerfer characteristic = case characteristic of
  "名词" -> "brown lighten-2"
  "动词" -> "orange lighten-2"
  "形容词" -> "red lighten-2"
  "介词" -> "lime lighten-2"
  "连词" -> "teal lighten-2"
  "代词" -> "pink lighten-2"
  "词缀" -> "indigo lighten-2"
  "副词" -> "purple lighten-2"
  _ -> "amber lighten-2 text-darken-2"


levelColor :: String -> String
levelColor lv = case lv of
  "1" -> "grey lighten-5" <> " black-text"
  "2" -> "grey lighten-4" <> " black-text"
  "3" -> "grey lighten-3" <> " black-text"
  "4" -> "grey lighten-2" <> " black-text"
  "5" -> "grey lighten-1" <> " black-text"
  "6" -> "grey"
  "7" -> "grey darken-1"
  "8" -> "grey darken-2"
  "9" -> "grey darken-3"
  "10" -> "grey darken-4"
  _ -> "transparent"

renderMeanings :: Array {meaning :: String, example :: Maybe String} -> R.ReactElement
renderMeanings ms =
  let
    renderMeaning m = R.li [RP.className "section"]
      [ R.div [RP.className "white-text"] [R.div [RP.className ""] [R.text m.meaning]]
      , R.p [RP.className "text-darken-2"] $ renderExample m.example
      ]

    renderExample e = case e of
      Nothing -> []
      Just ee -> [R.text ee]

  in
    R.ul [RP.className "", RP.property ""] $ renderMeaning <$> ms

myFooter :: R.ReactElement -> R.ReactElement
myFooter links =
    R.footer [ RP.className "page-footer" ]
        [ R.div [ RP.className "container" ]
            [ R.div [ RP.className "row" ]
                [ R.div [ RP.className "col l6 s12" ]
                    [ R.h5 [ RP.className "white-text" ] [ R.text "About" ]
                    , R.p [ RP.className "grey-text text-lighten-4" ] [ R.text "Simplingua is a simple language." ]
                    ]
                , R.div
                    [ RP.className "col l4 offset-l2 s12" ]
                    [ R.h5 [ RP.className "white-text" ] [ R.text "Extra Links" ]
                    , links
                    ]
                ]
            ]
        , R.div [ RP.className "footer-copyright" ]
            [ R.div [ RP.className "container" ]
                [ R.text "© 2016-2017 Simplingua BETA 0.3.1"
                , R.a [ RP.className "grey-text text-lighten-4 right", RP.href "#!" ] [ R.text "More Details" ]
                ]
            ]
        ]

linkList :: String -> String -> String -> R.ReactElement
linkList grammar pronunciation docDic = R.ul []
  [ R.li [] [ R.a [ RP.className "grey-text text-lighten-3", RP.href grammar ] [ R.text "Grammar" ] ]
  , R.li [] [ R.a [ RP.className "grey-text text-lighten-3", RP.href pronunciation ] [ R.text "Pronunciation" ] ]
  , R.li [] [ R.a [ RP.className "grey-text text-lighten-3", RP.href docDic ] [ R.text "Dictionary" ] ]
  ]

mainGui
  :: forall eff
  .  Array Dictionary
  -> Eff (dom :: DOM.DOM | eff) Unit
mainGui dict =
  myMain spec initialState ""
    where

      performAction :: forall e p . PerformAction e MyState p MyAction
      performAction (Query queryString) _ _ = void $ writeState $ take 100 $ queryDict queryString dict  -- queryString

      initialState :: MyState
      initialState = []

      spec :: forall e p . Spec e MyState p MyAction
      spec = simpleSpec performAction render

      myMain s ss props = void do
          let component = createClass s ss
          doc <- DOM.window >>= DOM.document
          con <- DOM.getElementById (DOM.ElementId "app") (DOM.htmlDocumentToNonElementParentNode doc)
          traverse_ (R.render (R.createFactory component props)) con
