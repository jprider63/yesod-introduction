module Handler.Register where

import Data.Char (isAlphaNum)
import qualified Data.Text as Text
import Yesod.Form.Bootstrap3

import Import

registerForm :: Form User
registerForm = renderBootstrap3 BootstrapBasicForm $ User
    <$> areq usernameField (bfs ("Username" :: Text)) Nothing
    <*> aopt emailField (bfs ("Email" :: Text)) Nothing

    where
        usernameField = check validateUsername textField

        validateUsername u | Text.all isAlphaNum u = Right u
        validateUsername _ = Left ("Usernames must be alphanumeric." :: Text)

generateHtml :: Widget -> Enctype -> Handler Html
generateHtml form enctype = defaultLayout $ do
    setTitle "Register"
    [whamlet|
        <h1>
            Register
        <form .form-basic role=form method=post action="@{RegisterR}" enctype=#{enctype}>
            ^{form}
            <div .form-group>
                <button .btn .btn-primary .btn-lg .btn-block type="submit">
                    Register
    |]
    

getRegisterR :: Handler Html
getRegisterR = do
    (form, enctype) <- generateFormPost registerForm
    generateHtml form enctype

postRegisterR :: Handler Html
postRegisterR = do
    ((res, form), enctype) <- runFormPost registerForm
    case res of
        FormSuccess user@(User _username _email) -> do
            runDB $ insert_ user

            setMessage "Signed up!"

            redirect HomeR
        _ -> do
            -- Error case.
            generateHtml form enctype

