let
    proj = mgis[proj],
    radius = 6371000,
    definition = (ellipsoid) => [type = "GeographicCRS", name = "Test sphere",
        datum = [type = "GeodeticReferenceFrame", name = "Test spherical datum", ellipsoid = ellipsoid],
        coordinate_system = [subtype = "ellipsoidal", axis = {
            [name = "Longitude", direction = "east", unit = "degree"],
            [name = "Latitude", direction = "north", unit = "degree"]}]],
    sphereDefinition = definition([name = "Sphere", radius = radius]),
    sphere = proj[fromJSON](sphereDefinition),
    fromText = proj[fromJSON](Text.FromBinary(Json.FromValue(sphereDefinition))),
    projectedDefinition = [type = "ProjectedCRS", name = "Spherical Mercator",
        base_crs = sphereDefinition,
        conversion = [name = "Mercator", method = [name = "Mercator (variant A)",
            id = [authority = "EPSG", code = 9804]], parameters = {}],
        coordinate_system = [subtype = "Cartesian", axis = {
            [name = "Easting", direction = "east", unit = "metre"],
            [name = "Northing", direction = "north", unit = "metre"]}]],
    projected = proj[fromJSON](projectedDefinition),
    expected = {radius * 2 * Number.PI / 180,
        radius * Number.Log(Number.Tan(Number.PI / 4 + 49 * Number.PI / 360)), 123},
    close = (actual, reference) => List.Count(actual) = List.Count(reference)
        and List.AllTrue(List.Transform(List.Positions(reference),
            (i) => Number.Abs(actual{i} - reference{i}) < 0.000001)),
    ensembleDefinition = Record.RemoveFields(Record.AddField(sphereDefinition,
        "datum_ensemble", sphereDefinition[datum]), {"datum"}),
    checks = [
        RadiusNormalizesToSphere = sphere[Ellipsoid][a] = radius
            and sphere[Ellipsoid][b] = radius and sphere[Ellipsoid][rf] = 0
            and sphere[Ellipsoid][e2] = 0 and sphere[DatumEllipsoid] = sphere[Ellipsoid],
        JSONTextMatchesRecord = fromText = sphere,
        DatumEnsembleSphere = proj[fromJSON](ensembleDefinition)[Ellipsoid] = sphere[Ellipsoid],
        RadiusWithMetreUnit = proj[fromJSON](definition([name = "Sphere",
            radius = [value = radius, unit = "metre"]]))[Ellipsoid] = sphere[Ellipsoid],
        RadiusWithFootUnit = Number.Abs(proj[fromJSON](definition([name = "Sphere",
            radius = [value = radius / 0.3048, unit = "foot"]]))[Ellipsoid][a] - radius) < 0.000001,
        RadiusWithCustomUnit = proj[fromJSON](definition([name = "Sphere",
            radius = [value = 6371, unit = [type = "LinearUnit", name = "kilometre",
                conversion_factor = 1000]]]))[Ellipsoid] = sphere[Ellipsoid],
        SphericalMercatorForward = close(proj[transform]({2, 49, 123}, sphere, projected), expected),
        SphericalMercatorInverse = close(proj[transform](expected, projected, sphere), {2, 49, 123}),
        ExistingEqualAxesSphere = proj[fromJSON](definition([name = "Sphere",
            semi_major_axis = radius, semi_minor_axis = radius]))[Ellipsoid] = sphere[Ellipsoid],
        ExistingZeroFlatteningSphere = proj[fromJSON](definition([name = "Sphere",
            semi_major_axis = radius, inverse_flattening = 0]))[Ellipsoid] = sphere[Ellipsoid],
        InvalidRadiusRejected = List.AllTrue(List.Transform({0, -1, #infinity, #nan},
            each (try proj[fromJSON](definition([name = "Sphere", radius = _]))[Ellipsoid][a])[HasError])),
        ConflictingRadiusRejected = (try proj[fromJSON](definition([name = "Sphere",
            radius = radius, semi_major_axis = radius, inverse_flattening = 0]))[Ellipsoid][a])[HasError],
        MissingDatumTransformStillRejected = (try proj[transform]({2, 49}, sphere, "EPSG:4326"))[HasError]
    ],
    evaluated = Record.FromList(List.Transform(Record.FieldNames(checks), (name) =>
        let result = try Record.Field(checks, name)
        in if result[HasError] then result[Error][Message] else result[Value]), Record.FieldNames(checks))
in
    if List.AllTrue(List.Transform(Record.FieldValues(evaluated), each _ = true)) then evaluated
    else error Error.Record("TestFailure", Text.FromBinary(Json.FromValue(evaluated)), null)
