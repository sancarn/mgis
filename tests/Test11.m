let
    proj = mgis[proj],
    wgs = proj[fromEPSG][#"EPSG:4326"],
    web = proj[fromEPSG][#"EPSG:3857"],
    // EPSG.io's full EPSG:3857 PROJ4 export, including its metadata flag.
    definition = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs +type=crs",
    exported = proj[fromProj4](definition),
    point = {2, 49, 123},
    expected = {222638.98158654713, 6274861.394006576, 123},
    close = (actual, reference) => List.Count(actual) = List.Count(reference)
        and List.AllTrue(List.Transform(List.Positions(reference),
            (i) => Number.Abs(actual{i} - reference{i}) < 0.000001)),
    identity = proj[fromProj4]("+proj=longlat +ellps=airy +nadgrids=@null"),
    checks = [
        EPSGioForward = close(proj[transform](point, wgs, exported), expected),
        EPSGioInverse = close(proj[transform](expected, exported, wgs), point),
        BuiltinAgreement = close(proj[transform](point, wgs, exported),
            proj[transform](point, wgs, web)),
        NullGridMetadata = exported[Grid] = "@null",
        NullGridBothDirections = close(proj[transform](point, identity, wgs), point)
            and close(proj[transform](point, wgs, identity), point),
        BareNullStillSupported = close(proj[transform](point,
            "+proj=longlat +ellps=airy +nadgrids=null", wgs), point),
        WKTextDoesNotChangeCoordinates = close(proj[transform](point, wgs,
            "+proj=merc +datum=WGS84 +wktext"),
            proj[transform](point, wgs, "+proj=merc +datum=WGS84")),
        RequiredGridStillRejected = (try proj[transform](point, wgs,
            "+proj=merc +ellps=airy +nadgrids=missing.tif,@null"))[HasError],
        OptionalGridStillRejected = (try proj[transform](point, wgs,
            "+proj=merc +ellps=airy +nadgrids=@missing.tif,@null"))[HasError]
    ],
    evaluated = Record.FromList(List.Transform(Record.FieldNames(checks), (name) =>
        let result = try Record.Field(checks, name)
        in if result[HasError] then result[Error][Message] else result[Value]), Record.FieldNames(checks))
in
    if List.AllTrue(List.Transform(Record.FieldValues(evaluated), each _ = true)) then evaluated
    else error Error.Record("TestFailure", Text.FromBinary(Json.FromValue(evaluated)), null)
