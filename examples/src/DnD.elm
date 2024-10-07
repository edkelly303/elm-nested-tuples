module DnD exposing (main)

import Browser
import Composer
import DnDList
import Html
import Html.Attributes
import Process
import Task



-- MAIN


main :
    Program
        ()
        ( List Fruit, ( Model, () ) )
        ( Maybe AppMsg, ( Maybe Msg, () ) )
main =
    Browser.element program


program =
    Composer.defineApp app
        |> Composer.addComponent component
        |> Composer.done


app =
    { init = \sendToDnD sendToSelf flags -> ( data, Cmd.none )
    , update =
        \sendToDnD sendToSelf msg model ->
            case msg of
                ItemsUpdated items ->
                    ( items, Cmd.none )
    , view =
        \viewDnD sendToDnD sendToSelf model ->
            Html.div []
                [ viewDnD
                , Html.text (Debug.toString model)
                ]
    , subscriptions = \sendToDnD sendToSelf model -> Sub.none
    }


type AppMsg
    = ItemsUpdated (List Fruit)


component =
    { init =
        \sendToApp sendToSelf flags ->
            init flags
                |> Tuple.mapSecond (Cmd.map sendToSelf)
    , update =
        update
    , view =
        \sendToApp sendToSelf model ->
            view model
                |> Html.map sendToSelf
    , subscriptions =
        \sendToApp sendToSelf model ->
            subscriptions model
                |> Sub.map sendToSelf
    }



-- DATA


type alias Fruit =
    String


data : List Fruit
data =
    [ "Apples", "Bananas", "Cherries", "Dates" ]



-- SYSTEM


config : DnDList.Config Fruit
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


system : DnDList.System Fruit Msg
system =
    DnDList.create config MyMsg



-- MODEL


type alias Model =
    { dnd : DnDList.Model
    , items : List Fruit
    }


initialModel : Model
initialModel =
    { dnd = system.model
    , items = data
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    system.subscriptions model.dnd



-- UPDATE


type Msg
    = MyMsg DnDList.Msg


update sendToApp sendToSelf message model =
    case message of
        MyMsg msg ->
            let
                ( dnd, items ) =
                    system.update msg model.dnd model.items
            in
            ( { model | dnd = dnd, items = items }
            , Cmd.batch
                [ Cmd.map sendToSelf (system.commands dnd)
                , Cmd.map sendToApp (Task.perform (\_ -> ItemsUpdated items) (Process.sleep 0))
                ]
            )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.section
        [ Html.Attributes.style "text-align" "center" ]
        [ model.items
            |> List.indexedMap (itemView model.dnd)
            |> Html.div []
        , ghostView model.dnd model.items
        ]


itemView : DnDList.Model -> Int -> Fruit -> Html.Html Msg
itemView dnd index item =
    let
        itemId : String
        itemId =
            "id-" ++ item
    in
    case system.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                Html.p
                    (Html.Attributes.id itemId :: system.dropEvents index itemId)
                    [ Html.text item ]

            else
                Html.p
                    [ Html.Attributes.id itemId ]
                    [ Html.text "[---------]" ]

        Nothing ->
            Html.p
                (Html.Attributes.id itemId :: system.dragEvents index itemId)
                [ Html.text item ]


ghostView : DnDList.Model -> List Fruit -> Html.Html Msg
ghostView dnd items =
    let
        maybeDragItem : Maybe Fruit
        maybeDragItem =
            system.info dnd
                |> Maybe.andThen (\{ dragIndex } -> items |> List.drop dragIndex |> List.head)
    in
    case maybeDragItem of
        Just item ->
            Html.div
                (system.ghostStyles dnd)
                [ Html.text item ]

        Nothing ->
            Html.text ""
