module UI (setupUI) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements as E
import Prelude hiding (on)
import AppState


setupUI :: IORef AppState -> Window -> UI ()
setupUI _ window = void $ mdo
  --set window title
  void $ return window # set title "Periferico Helper"
  E.addStyleSheet window "semantic.css"

  let idDiv =
        UI.div
          #. "field"
          #+ [ mkElement "label" # set UI.text "Order ID" # set (UI.attr "placeholder") "ID",
               mkElement "input"
             ]

  let nameDiv =
        UI.div
          #. "field"
          #+ [ mkElement "label" # set UI.text "Name",
               mkElement "input"
             ]

  let submitButton =
        UI.button
          #. "ui button"
          # set UI.type_ "submit"
          # set UI.text "Submit"

  let form =
        UI.div
          #. "ui form"
          #+ [idDiv, nameDiv, submitButton]

  let container =
        UI.div
          #. "ui container"
          #+ [form]

  getBody window #+ [container]

-- <form class="ui form">
--   <div class="field">
--     <label>First Name</label>
--     <input type="text" name="first-name" placeholder="First Name">
--   </div>
--   <div class="field">
--     <label>Last Name</label>
--     <input type="text" name="last-name" placeholder="Last Name">
--   </div>
--   <div class="field">
--     <div class="ui checkbox">
--       <input type="checkbox" tabindex="0" class="hidden">
--       <label>I agree to the Terms and Conditions</label>
--     </div>
--   </div>
--   <button class="ui button" type="submit">Submit</button>
-- </form>
