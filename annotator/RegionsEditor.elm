module RegionsEditor (..) where


import Html exposing (..)
import Html.Events
import List
import Maybe
import Signal
import Svg exposing (..)
import Svg.Events
import Svg.Attributes exposing (..)


import ListUtil
import Path exposing (Path, svgPathString)
import RegionEditor


ourHeight : Int
ourHeight = 240 * 2


ourWidth : Int
ourWidth = 352 * 2


type State
  = Editing Int RegionEditor.Model
  | NotEditing


editingIndex : State -> Maybe Int
editingIndex state =
  case state of
    Editing index _ -> Just index
    NotEditing -> Nothing


type alias Model =
  { state : State
  , paths : List Path
  , pictureIndex : Int
  }


init : Model
init =
  { state = NotEditing 
  , paths = []
  , pictureIndex = 0
  }


type Action
  = EditorAction RegionEditor.Action
  | NewRegion
  | StartEdit Int
  | StopEdit
  | SeekPicture Int


pictures : List String
pictures = [
  "1435858917071.jpg",
  "1435858918417.jpg",
  "1435858920034.jpg",
  "1435858921333.jpg",
  "1435858923079.jpg",
  "1435858924347.jpg",
  "1435858925949.jpg",
  "1435858927376.jpg",
  "1435858928944.jpg",
  "1435858930360.jpg",
  "1435858931832.jpg",
  "1435858933131.jpg",
  "1435858934676.jpg",
  "1435858936357.jpg",
  "1435858937812.jpg",
  "1435858939062.jpg",
  "1435858940543.jpg",
  "1435858942221.jpg",
  "1435858943717.jpg",
  "1435858945160.jpg",
  "1435858946507.jpg",
  "1435858948302.jpg",
  "1435858949608.jpg",
  "1435858951228.jpg",
  "1435858952433.jpg",
  "1435858954278.jpg",
  "1435858955484.jpg",
  "1435858957326.jpg",
  "1435858958562.jpg",
  "1435858960142.jpg",
  "1435858961361.jpg",
  "1435858963205.jpg",
  "1435858964422.jpg",
  "1435858966012.jpg",
  "1435858967437.jpg",
  "1435858969143.jpg",
  "1435858970583.jpg",
  "1435858971985.jpg",
  "1435858973379.jpg",
  "1435858975257.jpg",
  "1435858976695.jpg",
  "1435858978179.jpg",
  "1435858979422.jpg",
  "1435858981158.jpg",
  "1435858982604.jpg",
  "1435858984230.jpg",
  "1435858985493.jpg",
  "1435858987202.jpg",
  "1435858988660.jpg",
  "1435858990051.jpg",
  "1435858991441.jpg",
  "1435858993146.jpg",
  "1435858994650.jpg",
  "1435858996171.jpg",
  "1435858997426.jpg",
  "1435858999430.jpg",
  "1435859000861.jpg",
  "1435859002267.jpg",
  "1435859003498.jpg",
  "1435859005380.jpg",
  "1435859006775.jpg",
  "1435859008076.jpg",
  "1435859009297.jpg",
  "1435859011460.jpg",
  "1435859013332.jpg",
  "1435859014648.jpg",
  "1435859016254.jpg",
  "1435859017708.jpg",
  "1435859019420.jpg",
  "1435859020854.jpg",
  "1435859022253.jpg",
  "1435859023467.jpg",
  "1435859025082.jpg",
  "1435859027296.jpg",
  "1435859028504.jpg",
  "1435859030213.jpg",
  "1435859031604.jpg",
  "1435859033367.jpg",
  "1435859034824.jpg",
  "1435859036328.jpg",
  "1435859037528.jpg",
  "1435859039132.jpg",
  "1435859040508.jpg",
  "1435859041877.jpg",
  "1435859043230.jpg",
  "1435859044721.jpg",
  "1435859046255.jpg",
  "1435859047759.jpg",
  "1435859049159.jpg",
  "1435859050387.jpg",
  "1435859052164.jpg",
  "1435859053592.jpg",
  "1435859055268.jpg",
  "1435859056491.jpg",
  "1435859058250.jpg",
  "1435859059466.jpg",
  "1435859061294.jpg",
  "1435859062502.jpg",
  "1435859064010.jpg",
  "1435859065448.jpg",
  "1435859067099.jpg",
  "1435859068519.jpg",
  "1435859070008.jpg",
  "1435859071357.jpg",
  "1435859072827.jpg",
  "1435859074342.jpg",
  "1435859075847.jpg",
  "1435859077322.jpg",
  "1435859078632.jpg",
  "1435859080449.jpg",
  "1435859081832.jpg",
  "1435859083415.jpg",
  "1435859084724.jpg",
  "1435859086362.jpg",
  "1435859087660.jpg",
  "1435859089252.jpg",
  "1435859090452.jpg",
  "1435859092372.jpg",
  "1435859093618.jpg",
  "1435859095574.jpg",
  "1435859096838.jpg",
  "1435859098469.jpg",
  "1435859099689.jpg",
  "1435859101444.jpg",
  "1435859102690.jpg",
  "1435859104497.jpg",
  "1435859105730.jpg",
  "1435859107631.jpg",
  "1435859108861.jpg",
  "1435859110738.jpg",
  "1435859111959.jpg",
  "1435859113742.jpg",
  "1435859115033.jpg",
  "1435859116992.jpg",
  "1435859118236.jpg",
  "1435859120141.jpg",
  "1435859121386.jpg",
  "1435859123285.jpg",
  "1435859124511.jpg",
  "1435859126394.jpg",
  "1435859127599.jpg",
  "1435859129476.jpg",
  "1435859130687.jpg",
  "1435859132843.jpg",
  "1435859134058.jpg",
  "1435859135902.jpg",
  "1435859137122.jpg",
  "1435859139005.jpg",
  "1435859140224.jpg",
  "1435859142247.jpg",
  "1435859143445.jpg",
  "1435859145202.jpg",
  "1435859146404.jpg",
  "1435859148204.jpg",
  "1435859149407.jpg",
  "1435859151334.jpg",
  "1435859152543.jpg",
  "1435859154690.jpg",
  "1435859155943.jpg",
  "1435859158158.jpg",
  "1435859159398.jpg",
  "1435859161636.jpg",
  "1435859162893.jpg",
  "1435859164666.jpg",
  "1435859165881.jpg",
  "1435859167732.jpg",
  "1435859169139.jpg",
  "1435859170887.jpg",
  "1435859172109.jpg",
  "1435859173880.jpg",
  "1435859175122.jpg",
  "1435859177021.jpg",
  "1435859178259.jpg",
  "1435859180157.jpg",
  "1435859181436.jpg",
  "1435859183266.jpg",
  "1435859184492.jpg",
  "1435859186247.jpg",
  "1435859187483.jpg",
  "1435859189561.jpg",
  "1435859190785.jpg",
  "1435859192584.jpg",
  "1435859193811.jpg",
  "1435859195572.jpg",
  "1435859196821.jpg",
  "1435859198608.jpg",
  "1435859199828.jpg",
  "1435859201624.jpg",
  "1435859202835.jpg",
  "1435859205659.jpg",
  "1435859206879.jpg",
  "1435859208867.jpg",
  "1435859210151.jpg",
  "1435859212336.jpg",
  "1435859213781.jpg",
  "1435859215946.jpg",
  "1435859217764.jpg",
  "1435859219106.jpg",
  "1435859220894.jpg",
  "1435859222219.jpg",
  "1435859223871.jpg",
  "1435859225209.jpg",
  "1435859226998.jpg",
  "1435859228314.jpg",
  "1435859230125.jpg",
  "1435859232749.jpg",
  "1435859234641.jpg",
  "1435859236042.jpg",
  "1435859237722.jpg",
  "1435859239107.jpg",
  "1435859241293.jpg",
  "1435859243336.jpg",
  "1435859245273.jpg",
  "1435859246803.jpg",
  "1435859248453.jpg",
  "1435859249883.jpg",
  "1435859251545.jpg",
  "1435859253259.jpg",
  "1435859254888.jpg",
  "1435859256325.jpg",
  "1435859257968.jpg",
  "1435859259373.jpg",
  "1435859260975.jpg",
  "1435859262414.jpg",
  "1435859264011.jpg",
  "1435859265444.jpg",
  "1435859267039.jpg",
  "1435859268460.jpg",
  "1435859270086.jpg",
  "1435859271509.jpg",
  "1435859273071.jpg",
  "1435859274495.jpg",
  "1435859276054.jpg",
  "1435859277477.jpg",
  "1435859279091.jpg",
  "1435859280491.jpg",
  "1435859282061.jpg",
  "1435859283502.jpg",
  "1435859285172.jpg",
  "1435859286580.jpg",
  "1435859288252.jpg",
  "1435859289713.jpg",
  "1435859291399.jpg",
  "1435859292805.jpg",
  "1435859294528.jpg",
  "1435859295967.jpg",
  "1435859297699.jpg",
  "1435859299132.jpg",
  "1435859300857.jpg",
  "1435859303966.jpg",
  "1435859306053.jpg",
  "1435859307483.jpg",
  "1435859309413.jpg",
  "1435859310866.jpg",
  "1435859313437.jpg",
  "1435859316758.jpg",
  "1435859318612.jpg",
  "1435859319943.jpg",
  "1435859321702.jpg",
  "1435859322998.jpg",
  "1435859324758.jpg",
  "1435859326449.jpg",
  "1435859328228.jpg",
  "1435859329544.jpg",
  "1435859331233.jpg",
  "1435859332558.jpg",
  "1435859334239.jpg",
  "1435859335564.jpg",
  "1435859337220.jpg",
  "1435859338547.jpg",
  "1435859340440.jpg",
  "1435859342658.jpg",
  "1435859344173.jpg",
  "1435859345695.jpg",
  "1435859347299.jpg",
  "1435859348767.jpg",
  "1435859350365.jpg",
  "1435859351848.jpg",
  "1435859353470.jpg",
  "1435859354953.jpg",
  "1435859356600.jpg",
  "1435859358060.jpg",
  "1435859359661.jpg",
  "1435859361562.jpg",
  "1435859363096.jpg",
  "1435859364578.jpg",
  "1435859366362.jpg",
  "1435859367908.jpg",
  "1435859369532.jpg",
  "1435859371044.jpg",
  "1435859372718.jpg",
  "1435859374174.jpg",
  "1435859375702.jpg",
  "1435859377159.jpg",
  "1435859378691.jpg",
  "1435859380151.jpg",
  "1435859381717.jpg",
  "1435859383234.jpg",
  "1435859384843.jpg",
  "1435859386308.jpg",
  "1435859387819.jpg",
  "1435859389315.jpg",
  "1435859390860.jpg",
  "1435859392418.jpg",
  "1435859393924.jpg",
  "1435859395463.jpg",
  "1435859397282.jpg",
  "1435859398861.jpg",
  "1435859400558.jpg",
  "1435859402089.jpg",
  "1435859403630.jpg",
  "1435859405179.jpg",
  "1435859406714.jpg",
  "1435859408208.jpg",
  "1435859409765.jpg",
  "1435859411265.jpg",
  "1435859412804.jpg",
  "1435859414292.jpg",
  "1435859415959.jpg",
  "1435859417484.jpg",
  "1435859419026.jpg",
  "1435859420408.jpg",
  "1435859422693.jpg",
  "1435859424410.jpg",
  "1435859425996.jpg",
  "1435859428454.jpg",
  "1435859429723.jpg",
  "1435859431608.jpg",
  "1435859432815.jpg",
  "1435859434709.jpg",
  "1435859435933.jpg",
  "1435859437839.jpg",
  "1435859439038.jpg",
  "1435859441001.jpg",
  "1435859442429.jpg",
  "1435859444867.jpg",
  "1435859447033.jpg",
  "1435859448276.jpg",
  "1435859450155.jpg",
  "1435859451383.jpg",
  "1435859454072.jpg",
  "1435859456348.jpg",
  "1435859458058.jpg",
  "1435859459669.jpg",
  "1435859461202.jpg",
  "1435859462739.jpg",
  "1435859464305.jpg",
  "1435859465801.jpg",
  "1435859467318.jpg",
  "1435859468796.jpg",
  "1435859470394.jpg",
  "1435859471900.jpg",
  "1435859473711.jpg",
  "1435859475196.jpg",
  "1435859476734.jpg",
  "1435859478212.jpg",
  "1435859479784.jpg",
  "1435859481254.jpg",
  "1435859482833.jpg",
  "1435859484357.jpg",
  "1435859486081.jpg",
  "1435859487596.jpg",
  "1435859489121.jpg",
  "1435859490587.jpg",
  "1435859492086.jpg",
  "1435859493599.jpg",
  "1435859495130.jpg",
  "1435859496593.jpg",
  "1435859498181.jpg",
  "1435859499734.jpg",
  "1435859501241.jpg",
  "1435859502795.jpg",
  "1435859504308.jpg",
  "1435859505788.jpg",
  "1435859507362.jpg",
  "1435859509176.jpg",
  "1435859510973.jpg",
  "1435859512433.jpg",
  "1435859514149.jpg",
  "1435859515480.jpg",
  "1435859517247.jpg",
  "1435859518644.jpg",
  "1435859520783.jpg",
  "1435859521997.jpg",
  "1435859523942.jpg",
  "1435859525158.jpg",
  "1435859527190.jpg",
  "1435859528439.jpg",
  "1435859530304.jpg",
  "1435859531615.jpg",
  "1435859533387.jpg",
  "1435859534620.jpg",
  "1435859537060.jpg",
  "1435859538866.jpg",
  "1435859540316.jpg",
  "1435859542121.jpg",
  "1435859543431.jpg",
  "1435859545230.jpg",
  "1435859546613.jpg",
  "1435859548298.jpg",
  "1435859549638.jpg",
  "1435859551481.jpg",
  "1435859553276.jpg",
  "1435859555783.jpg",
  "1435859557303.jpg",
  "1435859558967.jpg",
  "1435859560386.jpg",
  "1435859562147.jpg",
  "1435859563568.jpg",
  "1435859565305.jpg",
  "1435859566716.jpg",
  "1435859568306.jpg",
  "1435859569767.jpg",
  "1435859571337.jpg",
  "1435859572775.jpg",
  "1435859574436.jpg",
  "1435859575838.jpg",
  "1435859577425.jpg",
  "1435859578930.jpg",
  "1435859581030.jpg",
  "1435859582462.jpg",
  "1435859584122.jpg",
  "1435859585690.jpg",
  "1435859587359.jpg",
  "1435859588921.jpg",
  "1435859590687.jpg",
  "1435859592208.jpg",
  "1435859594380.jpg",
  "1435859596172.jpg",
  "1435859597702.jpg",
  "1435859600156.jpg",
  "1435859601815.jpg",
  "1435859603351.jpg",
  "1435859604963.jpg",
  "1435859606721.jpg",
  "1435859609116.jpg",
  "1435859611857.jpg",
  "1435859615297.jpg",
  "1435859618056.jpg",
  "1435859620607.jpg",
  "1435859623643.jpg",
  "1435859627623.jpg",
  "1435859630076.jpg",
  "1435859631556.jpg",
  "1435859634569.jpg",
  "1435859638077.jpg",
  "1435859641587.jpg",
  "1435859645363.jpg",
  "1435859648992.jpg",
  "1435859652386.jpg",
  "1435859655249.jpg",
  "1435859657865.jpg",
  "1435859661702.jpg",
  "1435859665224.jpg",
  "1435859668766.jpg",
  "1435859672269.jpg",
  "1435859675374.jpg",
  "1435859678516.jpg",
  "1435859681146.jpg",
  "1435859683333.jpg",
  "1435859686667.jpg",
  "1435859693946.jpg",
  "1435859696439.jpg",
  "1435859697901.jpg",
  "1435859700354.jpg",
  "1435859703399.jpg",
  "1435859709592.jpg",
  "1435859711336.jpg",
  "1435859714257.jpg",
  "1435859721034.jpg",
  "1435859723682.jpg",
  "1435859726250.jpg",
  "1435859728714.jpg",
  "1435859732021.jpg",
  "1435859733868.jpg",
  "1435859736211.jpg",
  "1435859739691.jpg",
  "1435859742967.jpg",
  "1435859746383.jpg",
  "1435859749838.jpg",
  "1435859753298.jpg",
  "1435859757047.jpg",
  "1435859761427.jpg",
  "1435859765163.jpg",
  "1435859769196.jpg",
  "1435859770469.jpg",
  "1435859772251.jpg",
  "1435859773480.jpg",
  "1435859775251.jpg",
  "1435859776475.jpg",
  "1435859778348.jpg",
  "1435859779566.jpg",
  "1435859781462.jpg",
  "1435859782682.jpg",
  "1435859784547.jpg",
  "1435859785768.jpg",
  "1435859787636.jpg",
  "1435859788885.jpg",
  "1435859790714.jpg",
  "1435859791919.jpg",
  "1435859793685.jpg",
  "1435859794899.jpg",
  "1435859796676.jpg",
  "1435859797899.jpg",
  "1435859799684.jpg",
  "1435859800887.jpg",
  "1435859802644.jpg",
  "1435859803889.jpg",
  "1435859805727.jpg",
  "1435859806937.jpg",
  "1435859808848.jpg",
  "1435859810058.jpg",
  "1435859811926.jpg",
  "1435859813144.jpg",
  "1435859814965.jpg",
  "1435859816208.jpg",
  "1435859818101.jpg",
  "1435859819396.jpg",
  "1435859821179.jpg",
  "1435859822447.jpg",
  "1435859824281.jpg",
  "1435859825487.jpg",
  "1435859827243.jpg",
  "1435859828488.jpg",
  "1435859830261.jpg",
  "1435859831482.jpg",
  "1435859833340.jpg",
  "1435859834638.jpg",
  "1435859836452.jpg",
  "1435859837776.jpg",
  "1435859839582.jpg",
  "1435859840865.jpg",
  "1435859842682.jpg",
  "1435859843919.jpg",
  "1435859845778.jpg",
  "1435859847039.jpg",
  "1435859848834.jpg",
  "1435859850063.jpg",
  "1435859851861.jpg",
  "1435859853176.jpg",
  "1435859855201.jpg",
  "1435859856675.jpg",
  "1435859858703.jpg",
  "1435859859918.jpg",
  "1435859861806.jpg",
  "1435859863051.jpg",
  "1435859864988.jpg",
  "1435859866212.jpg",
  "1435859867974.jpg",
  "1435859869236.jpg",
  "1435859871173.jpg",
  "1435859872397.jpg",
  "1435859874322.jpg",
  "1435859875528.jpg",
  "1435859877408.jpg",
  "1435859878612.jpg",
  "1435859880490.jpg",
  "1435859881702.jpg",
  "1435859883536.jpg",
  "1435859884851.jpg",
  "1435859886755.jpg",
  "1435859888023.jpg",
  "1435859890009.jpg",
  "1435859891263.jpg",
  "1435859893090.jpg",
  "1435859894327.jpg",
  "1435859896170.jpg",
  "1435859897385.jpg",
  "1435859899294.jpg",
  "1435859900605.jpg",
  "1435859902374.jpg",
  "1435859903625.jpg",
  "1435859905412.jpg",
  "1435859907375.jpg",
  "1435859909384.jpg",
  "1435859910677.jpg",
  "1435859912965.jpg",
  "1435859914204.jpg",
  "1435859916115.jpg",
  "1435859917384.jpg",
  "1435859919257.jpg",
  "1435859920565.jpg",
  "1435859922540.jpg",
  "1435859923856.jpg",
  "1435859925714.jpg",
  "1435859927098.jpg",
  "1435859928974.jpg",
  "1435859930252.jpg",
  "1435859932110.jpg",
  "1435859933362.jpg",
  "1435859935546.jpg",
  "1435859936809.jpg",
  "1435859939184.jpg",
  "1435859940837.jpg",
  "1435859942346.jpg",
  "1435859944617.jpg",
  "1435859945848.jpg",
  "1435859947789.jpg",
  "1435859949106.jpg",
  "1435859950941.jpg",
  "1435859952233.jpg",
  "1435859954108.jpg",
  "1435859955385.jpg",
  "1435859957353.jpg",
  "1435859958631.jpg",
  "1435859960570.jpg",
  "1435859961780.jpg",
  "1435859963766.jpg",
  "1435859965006.jpg",
  "1435859966788.jpg",
  "1435859968016.jpg",
  "1435859970147.jpg",
  "1435859971954.jpg",
  "1435859973659.jpg",
  "1435859975725.jpg",
  "1435859977019.jpg",
  "1435859979009.jpg",
  "1435859980254.jpg",
  "1435859982163.jpg",
  "1435859983416.jpg"
  ]


region : Signal.Address Action -> Int -> Path -> Svg
region address index path =
  Svg.path
    [ d (svgPathString path)
    , fill "none"
    , stroke "red"
    , strokeWidth "2"
    , pointerEvents "visible"
    , Svg.Events.onClick (Signal.message address (StartEdit index))
    ]
    []

regions : Signal.Address Action -> Model -> Svg
regions address model =
  g
    []
    (model.paths
      |> List.indexedMap (,)
      |> List.filter (\(i, x) -> Just i /= editingIndex model.state)
      |> List.map (\(i, x) -> region address i x))


view : Signal.Address Action -> Model -> Html
view address model =
  let
    maybeEditor = case model.state of
      Editing _ editorModel ->
        [ RegionEditor.view
          (Signal.forwardTo address EditorAction)
          editorModel
        ]
      _ ->
        []
    buttons = case model.state of
      Editing _ _ ->
        [ button [ Html.Events.onClick address StopEdit ] [ Html.text "Stop Editing" ] ]
      NotEditing ->
        [ button [ Html.Events.onClick address NewRegion ] [ Html.text "New Region" ] ]
  in
    div
      []
      (
        [ Svg.svg
            [ width (toString ourWidth) 
            , height (toString ourHeight) 
            ]
            (
              [ image
                  [ xlinkHref ("images/" ++ Maybe.withDefault "" (ListUtil.get model.pictureIndex pictures))
                  , x "0"
                  , y "0"
                  , width (toString ourWidth)
                  , height (toString ourHeight)
                  ]
                  []
              , regions address model 
              ] ++ maybeEditor)
        , button [ Html.Events.onClick address (SeekPicture -2) ] [ Html.text "Previous Picture" ]
        , button [ Html.Events.onClick address (SeekPicture 2) ] [ Html.text "Next Picture" ]
        ] ++ buttons) 


update : Action -> Model -> Model
update action model =
  case action of
    EditorAction editorAction ->
      case model.state of
        Editing index editorModel ->
          let
            newEditorModel = RegionEditor.update editorAction editorModel
            newPaths = ListUtil.set index newEditorModel.path model.paths 
          in
            { model | state = Editing index newEditorModel, paths = newPaths }
        _ ->
          model
    NewRegion ->
      { model | paths = Path.empty :: model.paths, state = Editing 0 (RegionEditor.init Path.empty) }
    StartEdit index ->
      case ListUtil.get index model.paths of
        Just targetPath ->
          { model | state = Editing index (RegionEditor.init targetPath) }
        Nothing ->
          model
    StopEdit ->
      { model | state = NotEditing }
    SeekPicture seek ->
      { model | pictureIndex = model.pictureIndex + seek }
