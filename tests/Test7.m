let
    proj=mgis[proj],
    close=(actual,expected,tolerance)=>List.Count(actual)=List.Count(expected) and
        List.AllTrue(List.Transform(List.Positions(expected),(i)=>Number.Abs(actual{i}-expected{i})<=tolerance)),
    refs=projectionReferences[transforms],
    results=List.Transform(refs,(ref)=>let
        actual=proj[transform](ref[point],ref[source],ref[target],[allowApproximateDatum=ref[approximate]])
        in [source=ref[source],target=ref[target],point=ref[point],actual=actual,expected=ref[expected],pass=close(actual,ref[expected],ref[tolerance])]),
    json=List.Transform(projectionReferences[jsonCRS],(ref)=>[code=ref[code],crs=proj[fromJSON](ref[definition])]),
    checks=[
        IndependentCoordinates=List.AllTrue(List.Transform(results,each [pass])),
        BuiltinEPSG=Record.FieldNames(proj[fromEPSG])={"EPSG:4326","EPSG:3857","EPSG:27700"},
        JSONGeographic=List.First(List.Select(json,each [code]=4326))[crs][IsGeographic],
        JSONWebMercator=close(proj[transform]({2,49},"EPSG:4326",List.First(List.Select(json,each [code]=3857))[crs]),{222638.98158654713,6274861.394006576},0.001),
        JSONUTM=close(proj[transform]({2.2945,48.8584},"EPSG:4326",List.First(List.Select(json,each [code]=32631))[crs]),{448252.0013753649,5411954.909947274},0.001),
        JSONBNG=close(proj[transform]({1.7179215806451,52.657570301933},"+proj=longlat +datum=OSGB36",List.First(List.Select(json,each [code]=27700))[crs]),{651409.902802228,313177.269918699},0.001),
        UnsupportedMethodRejected=(try proj[fromProj4]("+proj=robin +datum=WGS84")[Method])[HasError],
        UnsupportedParameterRejected=(try proj[fromProj4]("+proj=merc +datum=WGS84 +bogus=1")[Method])[HasError],
        UnknownEPSGRejected=(try proj[transform]({0,0},"EPSG:999999","EPSG:4326"))[HasError],
        DuplicateParameterRejected=(try proj[fromProj4]("+proj=merc +proj=tmerc")[Method])[HasError],
        InapplicableParameterRejected=(try proj[fromProj4]("+proj=tmerc +lat_1=30")[Method])[HasError],
        GeographicScaleRejected=(try proj[fromProj4]("+proj=longlat +k=2")[Method])[HasError],
        InvalidEllipsoidRejected=(try proj[fromProj4]("+proj=merc +a=-1")[Method])[HasError],
        InvalidScaleRejected=(try proj[fromProj4]("+proj=merc +k=0")[Method])[HasError],
        InvalidZoneRejected=(try proj[fromProj4]("+proj=utm +zone=61")[Method])[HasError],
        InvalidCoordinateRejected=(try proj[transform]({0,#nan},"EPSG:4326","EPSG:3857"))[HasError],
        PoleRejected=(try proj[transform]({0,90},"EPSG:4326","EPSG:3857"))[HasError],
        InvalidLatitudeRejected=(try proj[transform]({0,91},"EPSG:4326","EPSG:3857"))[HasError],
        MissingDatumRejected=(try proj[transform]({1,2},"+proj=longlat +ellps=airy","EPSG:4326"))[HasError],
        ApproximateDatumRequiresOptIn=(try proj[transform]({530000,180000},"EPSG:27700","EPSG:4326"))[HasError],
        MissingGridRejected=(try proj[transform]({530000,180000},"+proj=tmerc +ellps=airy +nadgrids=missing.tif","EPSG:4326",[allowApproximateDatum=true]))[HasError],
        TMOutsideDomainRejected=(try proj[transform]({80,0},"EPSG:4326","+proj=tmerc +datum=WGS84"))[HasError]
    ],
    failed=List.Select(results,each not [pass])
in
    if List.AllTrue(Record.FieldValues(checks)) then checks
    else error Error.Record("TestFailure",Text.FromBinary(Json.FromValue([checks=checks,failed=failed])),null)
