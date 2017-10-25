module Handler.Register where

import Yesod.Form.Bootstrap3

import Import

registerForm :: Form User
registerForm = renderBootstrap3 BootstrapBasicForm $ User
    <$> areq textField (bfs ("Username" :: Text)) Nothing
    <*> aopt emailField (bfs ("Email" :: Text)) Nothing

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

