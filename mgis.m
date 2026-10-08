let
    //**********************
    //* 🗺️ GIS LIBRARY 🗺️ *
    //**********************
    //@url: https://github.com/sancarn/mgis
    //@description: GIS library for the PowerQuery M-lang ecosystem.
    //@author: Sancarn

    //**************************************************
    // Geometric Base Type
    //**************************************************

    //Base types as outputted by Geometry.FromWellKnownText() base function.
    TBasePoint = type [
        Kind = Value.Type("POINT"),
        X = number,
        Y = number
    ],
    TBaseLineString = type [
        Kind = Value.Type("LINESTRING"),
        Points = {TBasePoint}
    ],
    TBasePolygon = type [
        Kind = Value.Type("POLYGON"),
        Rings = {TBaseLineString}
    ],
    TBaseMultiPoint = type [
        Kind = Value.Type("MULTIPOINT"),
        Components = {TBasePoint}
    ],
    TBaseMultiLineString = type [
        Kind = Value.Type("MULTILINESTRING"),
        Components = {TBaseLineString}
    ],
    TBaseMultiPolygon = type [
        Kind = Value.Type("MULTIPOLYGON"),
        Components = {TBasePolygon}
    ],
    TBaseGeometryCollection = type [
        Kind = Value.Type("GEOMETRYCOLLECTION"),
        Components = {any}                        //Hack: No union types in M-lang
    ],
    
    //Calculates the bounding envelope (min/max X and Y) for any geometry type
    //@param geometry as (TBasePoint | TBaseLineString | TBasePolygon | TBaseMultiPoint | TBaseMultiLineString | TBaseMultiPolygon | TBaseGeometryCollection) - The geometry object to calculate the envelope for
    //@returns TShapeEnvelope - A record with MinX, MinY, MaxX, MaxY representing the bounding box
    GeometryGetEnvelope = (geometry as any) as record => (
        //Recursive switch statement based on geometry.Kind
        let 
            envelopsToEnvelope = (envelopes as list) as record => (
                let
                    MinXs = List.Transform(envelopes, each _[MinX]),
                    MinYs = List.Transform(envelopes, each _[MinY]),
                    MaxXs = List.Transform(envelopes, each _[MaxX]),
                    MaxYs = List.Transform(envelopes, each _[MaxY])
                in [
                    MinX = List.Min(MinXs),
                    MinY = List.Min(MinYs),
                    MaxX = List.Max(MaxXs),
                    MaxY = List.Max(MaxYs)
                ]
            ),
            switch = [
                POINT = (geometry as record) => [
                    MinX = geometry[X],
                    MinY = geometry[Y],
                    MaxX = geometry[X],
                    MaxY = geometry[Y]
                ],
                LINESTRING = (geometry as record) => (
                    let
                        //Compute envelopes for each point recursively
                        envelopes = List.Transform(geometry[Points], each @GeometryGetEnvelope(_)),
                        //Extract mins and maxs
                        envelope = envelopsToEnvelope(envelopes)
                    in [
                        MinX = envelope[MinX],
                        MinY = envelope[MinY],
                        MaxX = envelope[MaxX],
                        MaxY = envelope[MaxY]
                    ]
                ),
                POLYGON = (geometry as record) => (
                    let
                        //Compute envelopes for each ring recursively
                        envelopes = List.Transform(geometry[Rings], each @GeometryGetEnvelope(_)),
                        //Extract mins and maxs
                        envelope = envelopsToEnvelope(envelopes)
                    in [
                        MinX = envelope[MinX],
                        MinY = envelope[MinY],
                        MaxX = envelope[MaxX],
                        MaxY = envelope[MaxY]
                    ]
                ),
                MULTIPOINT = (geometry as record) => (
                    let
                        //Compute envelopes for each component recursively
                        envelopes = List.Transform(geometry[Components], each @GeometryGetEnvelope(_)),
                        //Extract mins and maxs
                        envelope = envelopsToEnvelope(envelopes)
                    in [
                        MinX = envelope[MinX],
                        MinY = envelope[MinY],
                        MaxX = envelope[MaxX],
                        MaxY = envelope[MaxY]
                    ]
                ),
                MULTILINESTRING = (geometry as record) => (
                    let
                        //Compute envelopes for each component recursively
                        envelopes = List.Transform(geometry[Components], each @GeometryGetEnvelope(_)),
                        //Extract mins and maxs
                        envelope = envelopsToEnvelope(envelopes)    
                    in [
                        MinX = envelope[MinX],
                        MinY = envelope[MinY],
                        MaxX = envelope[MaxX],
                        MaxY = envelope[MaxY]
                    ]
                ),
                MULTIPOLYGON = (geometry as record) => (
                    let
                        //Compute envelopes for each component recursively
                        envelopes = List.Transform(geometry[Components], each @GeometryGetEnvelope(_)),
                        //Extract mins and maxs
                        envelope = envelopsToEnvelope(envelopes)
                    in [
                        MinX = envelope[MinX],
                        MinY = envelope[MinY],
                        MaxX = envelope[MaxX],
                        MaxY = envelope[MaxY]
                    ]
                ),
                GEOMETRYCOLLECTION = (geometry as record) => (
                    let
                        //Compute envelopes for each component recursively
                        envelopes = List.Transform(geometry[Components], each @GeometryGetEnvelope(_)),
                        //Extract mins and maxs
                        envelope = envelopsToEnvelope(envelopes)
                    in [
                        MinX = envelope[MinX],
                        MinY = envelope[MinY],
                        MaxX = envelope[MaxX],
                        MaxY = envelope[MaxY]
                    ]
                )
            ]
        in
            Record.Field(switch, geometry[Kind])(geometry)
    ),


    //Checks if a point is inside a polygon using the ray casting algorithm
    //@param point - The point to test
    //@param polygon - The polygon to test against
    //@returns - True if the point is inside the polygon, false otherwise
    GeometryPointInPolygon = (point as record, polygon as record) as logical => (
        let
            x = point[X],
            y = point[Y],
            rings = polygon[Rings],
            //Test against the outer ring (first ring)
            outerRing = rings{0},
            points = outerRing[Points],
            n = List.Count(points),
            
            //Ray casting algorithm
            crossings = List.Accumulate(
                List.Positions(points),
                0,
                (state as number, i as number) as number => (
                    let
                        j = if i = n - 1 then 0 else i + 1,
                        pi = points{i},
                        pj = points{j},
                        yi = pi[Y],
                        yj = pj[Y],
                        xi = pi[X],
                        xj = pj[X],
                        intersects = ((yi > y) <> (yj > y)) and (x < (xj - xi) * (y - yi) / (yj - yi) + xi)
                    in
                        if intersects then state + 1 else state
                )
            ),
            inOuterRing = Number.Mod(crossings, 2) = 1,
            
            //Check holes (remaining rings)
            inHole = if List.Count(rings) > 1 then
                List.MatchesAny(
                    List.Skip(rings, 1),
                    (holeRing as record) as logical => (
                        let
                            holePoints = holeRing[Points],
                            holeN = List.Count(holePoints),
                            holeCrossings = List.Accumulate(
                                List.Positions(holePoints),
                                0,
                                (state as number, i as number) as number => (
                                    let
                                        j = if i = holeN - 1 then 0 else i + 1,
                                        pi = holePoints{i},
                                        pj = holePoints{j},
                                        yi = pi[Y],
                                        yj = pj[Y],
                                        xi = pi[X],
                                        xj = pj[X],
                                        intersects = ((yi > y) <> (yj > y)) and (x < (xj - xi) * (y - yi) / (yj - yi) + xi)
                                    in
                                        if intersects then state + 1 else state
                                )
                            )
                        in
                            Number.Mod(holeCrossings, 2) = 1
                    )
                )
            else
                false
        in
            inOuterRing and not inHole
    ),
    
    //Checks if two line segments intersect
    //@param p1 - Start point of first segment
    //@param p2 - End point of first segment
    //@param p3 - Start point of second segment
    //@param p4 - End point of second segment
    //@returns - True if the segments intersect, false otherwise
    GeometrySegmentsIntersect = (p1 as record, p2 as record, p3 as record, p4 as record) as logical => (
        let
            x1 = p1[X], y1 = p1[Y],
            x2 = p2[X], y2 = p2[Y],
            x3 = p3[X], y3 = p3[Y],
            x4 = p4[X], y4 = p4[Y],
            
            denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4),
            
            result = if denom = 0 then
                //Segments are parallel or collinear
                false
            else
                let
                    t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom,
                    u = -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / denom
                in
                    t >= 0 and t <= 1 and u >= 0 and u <= 1
        in
            result
    ),
    
    //Checks if two geometries intersect
    //@param geom1 - The first geometry
    //@param geom2 - The second geometry
    //@returns - True if the geometries intersect, false otherwise
    GeometryIntersects = (geom1 as record, geom2 as record) as logical => (
        let
            kind1 = geom1[Kind],
            kind2 = geom2[Kind],
            
            //Point-Point intersection
            pointPointIntersects = (p1 as record, p2 as record) as logical =>
                p1[X] = p2[X] and p1[Y] = p2[Y],
            
            //Point-Polygon intersection
            pointPolygonIntersects = (point as record, polygon as record) as logical =>
                @GeometryPointInPolygon(point, polygon),
            
            //Point-LineString intersection
            pointLineStringIntersects = (point as record, linestring as record) as logical =>
                List.MatchesAny(
                    linestring[Points],
                    (p as record) as logical => p[X] = point[X] and p[Y] = point[Y]
                ),
            
            //LineString-LineString intersection
            lineStringLineStringIntersects = (ls1 as record, ls2 as record) as logical =>
                let
                    points1 = ls1[Points],
                    points2 = ls2[Points]
                in
                    List.MatchesAny(
                        List.Transform({0..List.Count(points1)-2}, each _),
                        (i as number) as logical =>
                            List.MatchesAny(
                                List.Transform({0..List.Count(points2)-2}, each _),
                                (j as number) as logical =>
                                    @GeometrySegmentsIntersect(points1{i}, points1{i+1}, points2{j}, points2{j+1})
                            )
                    ),
            
            //LineString-Polygon intersection
            lineStringPolygonIntersects = (linestring as record, polygon as record) as logical =>
                let
                    points = linestring[Points],
                    //Check if any vertex is inside the polygon
                    anyPointInside = List.MatchesAny(points, (p as record) as logical => @GeometryPointInPolygon(p, polygon)),
                    //Check if any segment intersects the polygon boundary
                    rings = polygon[Rings],
                    anySegmentCrossesBoundary = List.MatchesAny(
                        List.Transform({0..List.Count(points)-2}, each _),
                        (i as number) as logical =>
                            List.MatchesAny(
                                rings,
                                (ring as record) as logical =>
                                    let
                                        ringPoints = ring[Points]
                                    in
                                        List.MatchesAny(
                                            List.Transform({0..List.Count(ringPoints)-2}, each _),
                                            (j as number) as logical =>
                                                @GeometrySegmentsIntersect(points{i}, points{i+1}, ringPoints{j}, ringPoints{j+1})
                                        )
                            )
                    )
                in
                    anyPointInside or anySegmentCrossesBoundary,
            
            //Polygon-Polygon intersection
            polygonPolygonIntersects = (poly1 as record, poly2 as record) as logical =>
                let
                    rings1 = poly1[Rings],
                    rings2 = poly2[Rings],
                    //Check if any vertex of poly1 is inside poly2
                    anyP1PointInP2 = List.MatchesAny(
                        rings1{0}[Points],
                        (p as record) as logical => @GeometryPointInPolygon(p, poly2)
                    ),
                    //Check if any vertex of poly2 is inside poly1
                    anyP2PointInP1 = List.MatchesAny(
                        rings2{0}[Points],
                        (p as record) as logical => @GeometryPointInPolygon(p, poly1)
                    ),
                    //Check if any edges intersect
                    anyEdgesIntersect = List.MatchesAny(
                        rings1,
                        (ring1 as record) as logical =>
                            let
                                points1 = ring1[Points]
                            in
                                List.MatchesAny(
                                    rings2,
                                    (ring2 as record) as logical =>
                                        let
                                            points2 = ring2[Points]
                                        in
                                            List.MatchesAny(
                                                List.Transform({0..List.Count(points1)-2}, each _),
                                                (i as number) as logical =>
                                                    List.MatchesAny(
                                                        List.Transform({0..List.Count(points2)-2}, each _),
                                                        (j as number) as logical =>
                                                            @GeometrySegmentsIntersect(points1{i}, points1{i+1}, points2{j}, points2{j+1})
                                                    )
                                            )
                                )
                    )
                in
                    anyP1PointInP2 or anyP2PointInP1 or anyEdgesIntersect,
            
            result = 
                if kind1 = "POINT" and kind2 = "POINT" then
                    pointPointIntersects(geom1, geom2)
                else if kind1 = "POINT" and kind2 = "POLYGON" then
                    pointPolygonIntersects(geom1, geom2)
                else if kind1 = "POLYGON" and kind2 = "POINT" then
                    pointPolygonIntersects(geom2, geom1)
                else if kind1 = "POINT" and kind2 = "LINESTRING" then
                    pointLineStringIntersects(geom1, geom2)
                else if kind1 = "LINESTRING" and kind2 = "POINT" then
                    pointLineStringIntersects(geom2, geom1)
                else if kind1 = "LINESTRING" and kind2 = "LINESTRING" then
                    lineStringLineStringIntersects(geom1, geom2)
                else if kind1 = "LINESTRING" and kind2 = "POLYGON" then
                    lineStringPolygonIntersects(geom1, geom2)
                else if kind1 = "POLYGON" and kind2 = "LINESTRING" then
                    lineStringPolygonIntersects(geom2, geom1)
                else if kind1 = "POLYGON" and kind2 = "POLYGON" then
                    polygonPolygonIntersects(geom1, geom2)
                else if kind1 = "MULTIPOINT" then
                    List.MatchesAny(geom1[Components], (comp as record) as logical => @GeometryIntersects(comp, geom2))
                else if kind2 = "MULTIPOINT" then
                    List.MatchesAny(geom2[Components], (comp as record) as logical => @GeometryIntersects(geom1, comp))
                else if kind1 = "MULTILINESTRING" then
                    List.MatchesAny(geom1[Components], (comp as record) as logical => @GeometryIntersects(comp, geom2))
                else if kind2 = "MULTILINESTRING" then
                    List.MatchesAny(geom2[Components], (comp as record) as logical => @GeometryIntersects(geom1, comp))
                else if kind1 = "MULTIPOLYGON" then
                    List.MatchesAny(geom1[Components], (comp as record) as logical => @GeometryIntersects(comp, geom2))
                else if kind2 = "MULTIPOLYGON" then
                    List.MatchesAny(geom2[Components], (comp as record) as logical => @GeometryIntersects(geom1, comp))
                else if kind1 = "GEOMETRYCOLLECTION" then
                    List.MatchesAny(geom1[Components], (comp as record) as logical => @GeometryIntersects(comp, geom2))
                else if kind2 = "GEOMETRYCOLLECTION" then
                    List.MatchesAny(geom2[Components], (comp as record) as logical => @GeometryIntersects(geom1, comp))
                else
                    false
        in
            result
    ),
    
    //Checks if geometry1 contains geometry2 (all points of geom2 are inside geom1)
    //@param geom1 - The containing geometry
    //@param geom2 - The geometry to test
    //@returns - True if geom1 contains geom2, false otherwise
    GeometryContains = (geom1 as record, geom2 as record) as logical => (
        let
            kind1 = geom1[Kind],
            kind2 = geom2[Kind],
            
            //Point contains point
            pointContainsPoint = (p1 as record, p2 as record) as logical =>
                p1[X] = p2[X] and p1[Y] = p2[Y],
            
            //Polygon contains point
            polygonContainsPoint = (polygon as record, point as record) as logical =>
                @GeometryPointInPolygon(point, polygon),
            
            //Polygon contains linestring (all points must be inside)
            polygonContainsLineString = (polygon as record, linestring as record) as logical =>
                List.MatchesAll(linestring[Points], (p as record) as logical => @GeometryPointInPolygon(p, polygon)),
            
            //Polygon contains polygon (all points of inner polygon must be in outer)
            polygonContainsPolygon = (outerPoly as record, innerPoly as record) as logical =>
                let
                    allPointsInside = List.MatchesAll(
                        innerPoly[Rings]{0}[Points],
                        (p as record) as logical => @GeometryPointInPolygon(p, outerPoly)
                    )
                in
                    allPointsInside,
            
            result =
                if kind1 = "POINT" and kind2 = "POINT" then
                    pointContainsPoint(geom1, geom2)
                else if kind1 = "POLYGON" and kind2 = "POINT" then
                    polygonContainsPoint(geom1, geom2)
                else if kind1 = "POLYGON" and kind2 = "LINESTRING" then
                    polygonContainsLineString(geom1, geom2)
                else if kind1 = "POLYGON" and kind2 = "POLYGON" then
                    polygonContainsPolygon(geom1, geom2)
                else if kind1 = "MULTIPOLYGON" then
                    List.MatchesAny(geom1[Components], (comp as record) as logical => @GeometryContains(comp, geom2))
                else if kind2 = "MULTIPOINT" then
                    List.MatchesAll(geom2[Components], (comp as record) as logical => @GeometryContains(geom1, comp))
                else if kind2 = "MULTILINESTRING" then
                    List.MatchesAll(geom2[Components], (comp as record) as logical => @GeometryContains(geom1, comp))
                else if kind2 = "MULTIPOLYGON" then
                    List.MatchesAll(geom2[Components], (comp as record) as logical => @GeometryContains(geom1, comp))
                else
                    false
        in
            result
    ),
    
    //Checks if geometry1 is within geometry2 (all points of geom1 are inside geom2)
    //@param geom1 - The geometry to test
    //@param geom2 - The containing geometry
    //@returns - True if geom1 is within geom2, false otherwise
    GeometryWithin = (geom1 as record, geom2 as record) as logical =>
        GeometryContains(geom2, geom1),

    //**************************************************
    // Shape Wrapper Type
    //**************************************************

    TShapeEnvelope = type [
        MinX = number,
        MinY = number,
        MaxX = number,
        MaxY = number
    ],
    TShape = type [
        //To ensure this is a TShape, not some other record, we add this field. It won't be used for any other purpose. Just a sanity check.
        __TShapeIdentifier__ = null,
        //Kind as "POINT" | "LINESTRING" | "POLYGON" | "MULTIPOINT" | "MULTILINESTRING" | "MULTIPOLYGON" | "GEOMETRYCOLLECTION"
        Kind = text,
        //Geometry as TBasePoint | TBaseLineString | TBasePolygon | TBaseMultiPoint | TBaseMultiLineString | TBaseMultiPolygon | TBaseGeometryCollection
        Geometry = any,  //Hack: No union types in M-lang
        Envelope = TShapeEnvelope,
        __rowid__ = nullable number
    ],
    TProjection = type record,

    //**************************************************
    // Pure-M coordinate reference systems and projections
    // Coordinates use GIS order: X=longitude/easting, Y=latitude/northing.
    // Angles in CRS definitions are degrees; projection kernels use radians.
    // See tests/data/projection-references.json for independent reference values.
    //**************************************************
    ProjPi = Number.PI,
    ProjRadians = Number.PI / 180,
    ProjError = (message as text) => error Error.Record("ProjectionError", message, null),
    ProjFinite = (x as any) as logical => Value.Is(x, type number)
        and not Number.IsNaN(x) and x <> #infinity and x <> -#infinity,
    ProjWrap = (x as number) as number => x - 2 * ProjPi * Number.RoundDown((x + ProjPi) / (2 * ProjPi)),
    ProjAsinh = (x as number) as number => Number.Sign(x) * Number.Ln(Number.Abs(x) + Number.Sqrt(x*x + 1)),
    ProjAtanh = (x as number) as number => Number.Ln((1+x)/(1-x))/2,
    ProjSinh = (x as number) as number => (Number.Exp(x) - Number.Exp(-x))/2,
    ProjCosh = (x as number) as number => (Number.Exp(x) + Number.Exp(-x))/2,
    ProjSolve = (initial as number, step as function, tolerance as number, limit as number) as number =>
        let
            // Buffer scalar state: an unconverged lazy record chain can otherwise
            // repeatedly evaluate earlier iterations in the Power Query engine.
            result = List.Accumulate({1..limit}, List.Buffer({initial,false}), (state, i) =>
                if state{1} then state else
                let next = step(state{0}) in
                    List.Buffer({next,Number.Abs(next-state{0}) <= tolerance}))
        in if result{1} and ProjFinite(result{0}) then result{0}
            else ProjError("Numerical iteration did not converge within its supported domain."),
    ProjEllipsoid = (a as number, rf as number) as record =>
        if not ProjFinite(a) or a <= 0 or not ProjFinite(rf) or (rf <> 0 and rf <= 1) then
            ProjError("Invalid ellipsoid: require a > 0 and inverse flattening > 1 (or 0 for a sphere).")
        else let f = if rf = 0 then 0 else 1/rf in
            [a=a, rf=rf, f=f, b=a*(1-f), e2=f*(2-f), e=Number.Sqrt(f*(2-f))],
    ProjEllipsoids = [
        WGS84 = ProjEllipsoid(6378137, 298.257223563),
        GRS80 = ProjEllipsoid(6378137, 298.257222101),
        airy = ProjEllipsoid(6377563.396, 299.3249646),
        intl = ProjEllipsoid(6378388, 297),
        bessel = ProjEllipsoid(6377397.155, 299.1528128),
        clrk66 = ProjEllipsoid(6378206.4, 294.978698214),
        sphere = ProjEllipsoid(6370997, 0)
    ],
    ProjDatums = [
        WGS84 = [ellipsoid=ProjEllipsoids[WGS84], toWGS84={0,0,0}, approximate=false],
        OSGB36 = [ellipsoid=ProjEllipsoids[airy], toWGS84={446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894}, approximate=true],
        NAD83 = [ellipsoid=ProjEllipsoids[GRS80], toWGS84=null, approximate=false]
    ],
    ProjUnits = [m=1, km=1000, cm=0.01, mm=0.001, ft=0.3048, #"us-ft"=1200/3937],
    ProjIsometric = (phi as number, ell as record) as number =>
        ProjAsinh(Number.Tan(phi)) - ell[e]*ProjAtanh(ell[e]*Number.Sin(phi)),
    ProjPhi = (q as number, ell as record) as number =>
        ProjSolve(2*Number.Atan(Number.Exp(q))-ProjPi/2,
            (phi) => 2*Number.Atan(Number.Exp(q+ell[e]*ProjAtanh(ell[e]*Number.Sin(phi))))-ProjPi/2,
            1e-13, 30),

    // Sixth-order Krueger series, with explicit domain checks rather than a
    // low-order TM approximation silently diverging far from the central meridian.
    ProjTMConstants = (ell as record) as record =>
        let
            n=ell[f]/(2-ell[f]), n2=n*n, n3=n2*n, n4=n3*n, n5=n4*n, n6=n5*n,
            alpha={
                n/2-2*n2/3+5*n3/16+41*n4/180-127*n5/288+7891*n6/37800,
                13*n2/48-3*n3/5+557*n4/1440+281*n5/630-1983433*n6/1935360,
                61*n3/240-103*n4/140+15061*n5/26880+167603*n6/181440,
                49561*n4/161280-179*n5/168+6601661*n6/7257600,
                34729*n5/80640-3418889*n6/1995840, 212378941*n6/319334400},
            beta={
                n/2-2*n2/3+37*n3/96-n4/360-81*n5/512+96199*n6/604800,
                n2/48+n3/15-437*n4/1440+46*n5/105-1118711*n6/3870720,
                17*n3/480-37*n4/840-209*n5/4480+5569*n6/90720,
                4397*n4/161280-11*n5/504-830251*n6/7257600,
                4583*n5/161280-108847*n6/3991680, 20648693*n6/638668800}
        in [A=ell[a]/(1+n)*(1+n2/4+n4/64+n6/256), alpha=alpha, beta=beta],
    ProjTMRaw = (lambda as number, phi as number, ell as record, c as record) as list =>
        let
            tau=Number.Tan(phi), sigma=ProjSinh(ell[e]*ProjAtanh(ell[e]*tau/Number.Sqrt(1+tau*tau))),
            tauPrime=tau*Number.Sqrt(1+sigma*sigma)-sigma*Number.Sqrt(1+tau*tau),
            xi=Number.Atan2(tauPrime, Number.Cos(lambda)),
            eta=ProjAsinh(Number.Sin(lambda)/Number.Sqrt(tauPrime*tauPrime+Number.Power(Number.Cos(lambda),2))),
            dx=List.Sum(List.Transform({1..6}, (j) => c[alpha]{j-1}*Number.Sin(2*j*xi)*ProjCosh(2*j*eta))),
            dy=List.Sum(List.Transform({1..6}, (j) => c[alpha]{j-1}*Number.Cos(2*j*xi)*ProjSinh(2*j*eta)))
        in if Number.Abs(lambda) >= ProjPi/2 or Number.Abs(eta)>1 then
            ProjError("Transverse Mercator coordinate is outside the supported series domain (|eta| <= 1, front hemisphere).")
            else {xi+dx, eta+dy},
    ProjLCCConstants = (p as record, ell as record) as record =>
        let
            phi1=p[lat1]*ProjRadians, phi2=p[lat2]*ProjRadians,
            m=(phi) => Number.Cos(phi)/Number.Sqrt(1-ell[e2]*Number.Power(Number.Sin(phi),2)),
            t=(phi) => Number.Exp(-ProjIsometric(phi,ell)),
            n=if Number.Abs(phi1-phi2)<1e-12 then Number.Sin(phi1)
                else Number.Ln(m(phi1)/m(phi2))/Number.Ln(t(phi1)/t(phi2)),
            F=m(phi1)/(n*Number.Power(t(phi1),n)),
            rho0=ell[a]*p[k]*F*Number.Power(t(p[lat0]*ProjRadians),n)
        in if Number.Abs(n)<1e-12 then ProjError("Lambert Conformal Conic requires non-degenerate standard parallels.")
            else [n=n,F=F,rho0=rho0],

    // Family registry: adding a projection does not require changing layer/join code.
    ProjMethods = [
        longlat = [
            forward=(ll,p,ell) => ll,
            inverse=(xy,p,ell) => xy
        ],
        merc = [
            forward=(ll,p,ell) => {ell[a]*p[k]*ProjWrap(ll{0}-p[lon0]*ProjRadians), ell[a]*p[k]*ProjIsometric(ll{1},ell)},
            inverse=(xy,p,ell) => {ProjWrap(xy{0}/(ell[a]*p[k])+p[lon0]*ProjRadians), ProjPhi(xy{1}/(ell[a]*p[k]),ell)}
        ],
        tmerc = [
            forward=(ll,p,ell) => let
                c=ProjTMConstants(ell), origin=ProjTMRaw(0,p[lat0]*ProjRadians,ell,c),
                v=ProjTMRaw(ProjWrap(ll{0}-p[lon0]*ProjRadians),ll{1},ell,c)
                in {p[k]*c[A]*v{1},p[k]*c[A]*(v{0}-origin{0})},
            inverse=(xy,p,ell) => let
                c=ProjTMConstants(ell), origin=ProjTMRaw(0,p[lat0]*ProjRadians,ell,c),
                xi=xy{1}/(p[k]*c[A])+origin{0}, eta=xy{0}/(p[k]*c[A]),
                xp=xi-List.Sum(List.Transform({1..6},(j)=>c[beta]{j-1}*Number.Sin(2*j*xi)*ProjCosh(2*j*eta))),
                ep=eta-List.Sum(List.Transform({1..6},(j)=>c[beta]{j-1}*Number.Cos(2*j*xi)*ProjSinh(2*j*eta))),
                lambda=Number.Atan2(ProjSinh(ep),Number.Cos(xp)),
                tau=Number.Sin(xp)/Number.Sqrt(Number.Power(ProjSinh(ep),2)+Number.Power(Number.Cos(xp),2)),
                phi=ProjPhi(ProjAsinh(tau),ell)
                in if Number.Abs(eta)>1.1 or Number.Abs(ep)>1 or Number.Abs(lambda)>=ProjPi/2 then
                    ProjError("Transverse Mercator coordinate is outside the supported inverse domain.")
                    else {ProjWrap(lambda+p[lon0]*ProjRadians),phi}
        ],
        lcc = [
            forward=(ll,p,ell) => let
                c=ProjLCCConstants(p,ell), theta=c[n]*ProjWrap(ll{0}-p[lon0]*ProjRadians),
                rho=ell[a]*p[k]*c[F]*Number.Exp(-c[n]*ProjIsometric(ll{1},ell))
                in {rho*Number.Sin(theta), c[rho0]-rho*Number.Cos(theta)},
            inverse=(xy,p,ell) => let
                c=ProjLCCConstants(p,ell), y=c[rho0]-xy{1}, sign=Number.Sign(c[n]),
                rho=sign*Number.Sqrt(xy{0}*xy{0}+y*y), theta=Number.Atan2(sign*xy{0},sign*y),
                t=Number.Power(rho/(ell[a]*p[k]*c[F]),1/c[n])
                in if rho=0 then {p[lon0]*ProjRadians,sign*ProjPi/2}
                    else {ProjWrap(theta/c[n]+p[lon0]*ProjRadians),ProjPhi(-Number.Ln(t),ell)}
        ]
    ],
    ProjCreate = (definition as record) as record =>
        let
            p=Record.Combine({[lat0=0,lon0=0,k=1,x0=0,y0=0,lat1=0,lat2=0],definition[parameters]}),
            method=definition[Method], ell=definition[Ellipsoid], unit=definition[UnitToMeter],
            valid=Record.HasFields(ProjMethods,method) and ell[a]>0 and ell[rf]>=0 and List.AllTrue(List.Transform(Record.FieldValues(p),ProjFinite))
                and p[k]>0 and Number.Abs(p[lat0])<90 and Number.Abs(p[lat1])<90 and Number.Abs(p[lat2])<90
                and ProjFinite(unit) and unit>0,
            checked=if not valid then ProjError("Unsupported projection family or invalid projection parameters/units.") else p,
            result=Record.Combine({definition,[parameters=checked,IsGeographic=method="longlat",__CRS__=true]})
        in if method="tmerc" and ell[f]>0.02 then ProjError("The Transverse Mercator series requires flattening <= 1/50.")
            else if method="merc" and checked[lat0]<>0 then ProjError("Mercator latitude of natural origin must be zero.")
            else if method="longlat" and (unit<>1 or checked<>[lat0=0,lon0=0,k=1,x0=0,y0=0,lat1=0,lat2=0]) then
                ProjError("Geographic CRSs must use degree coordinates without projected offsets or scale parameters.")
            else if method="lcc" then
            if ProjFinite(ProjLCCConstants(checked,ell)[n]) then result else ProjError("Invalid LCC definition.")
            else if checked[k]>0 then result else ProjError("Invalid CRS."),

    ProjFromProj4 = (text as text) as record =>
        let
            tokens=List.Select(Text.SplitAny(Text.Trim(text)," " & Character.FromNumber(9) & Character.FromNumber(10) & Character.FromNumber(13)),each _<>""),
            names=List.Transform(tokens,each Text.BeforeDelimiter(Text.TrimStart(_,"+") & "=","=")),
            values=List.Transform(tokens,each if Text.Contains(_,"=") then Text.AfterDelimiter(_,"=") else "true"),
            supported={"proj","datum","ellps","a","b","rf","f","R","lat_0","lon_0","lat_1","lat_2","lat_ts","k","k_0","x_0","y_0","units","to_meter","towgs84","nadgrids","no_defs","type","zone","south","axis","pm"},
            raw=if List.IsEmpty(tokens) or List.AnyTrue(List.Transform(tokens,each not Text.StartsWith(_,"+"))) then ProjError("Expected a +key=value PROJ4 definition.")
                else if List.Count(List.Distinct(names))<>List.Count(names) then ProjError("Duplicate PROJ4 parameter.")
                else if not List.IsEmpty(List.Difference(names,supported)) then ProjError("Unsupported PROJ4 parameters: " & Text.Combine(List.Difference(names,supported),", "))
                else Record.FromList(values,names),
            get=(key,default)=>Record.FieldOrDefault(raw,key,default),
            num=(key,default)=>if Record.HasFields(raw,key) then Number.FromText(Record.Field(raw,key),"en-US") else default,
            datumName=get("datum",null),
            datum=if datumName=null then null else if Record.HasFields(ProjDatums,datumName) then Record.Field(ProjDatums,datumName)
                else ProjError("Unsupported named datum. Supply explicit ellipsoid and +towgs84 parameters."),
            namedEll=get("ellps",null),
            named=if namedEll=null then (if datum=null then ProjEllipsoids[WGS84] else datum[ellipsoid])
                else if Record.HasFields(ProjEllipsoids,namedEll) then Record.Field(ProjEllipsoids,namedEll)
                else ProjError("Unknown ellipsoid. Supply +a and +rf (or +b)."),
            a=num("a",named[a]),
            rf=if Record.HasFields(raw,"b") then (if num("b",a)=a then 0 else a/(a-num("b",a)))
                else if Record.HasFields(raw,"f") then (if num("f",0)=0 then 0 else 1/num("f",0)) else num("rf",named[rf]),
            ell=if Record.HasFields(raw,"R") then ProjEllipsoid(num("R",0),0) else ProjEllipsoid(a,rf),
            inputMethod=get("proj",null), method=if List.Contains({"latlong","lonlat"},inputMethod) then "longlat"
                else if inputMethod="utm" then "tmerc" else if inputMethod="webmerc" then "merc" else inputMethod,
            projectionEll=if inputMethod="webmerc" then ProjEllipsoid(ell[a],0) else ell,
            zone=num("zone",0),
            params0=[lat0=num("lat_0",0),lon0=num("lon_0",0),k=num("k_0",num("k",1)),x0=num("x_0",0),y0=num("y_0",0),lat1=num("lat_1",0),lat2=num("lat_2",num("lat_1",0))],
            params=if inputMethod="utm" then
                if zone<1 or zone>60 or Number.RoundDown(zone)<>zone then ProjError("UTM zone must be an integer from 1 to 60.")
                else Record.Combine({params0,[lat0=0,lon0=zone*6-183,k=0.9996,x0=500000,y0=if Record.HasFields(raw,"south") then 10000000 else 0]})
                else if Record.HasFields(raw,"lat_ts") then
                    if method<>"merc" or Number.Abs(num("lat_ts",0))>=90 then ProjError("+lat_ts is supported for Mercator, between -90 and 90 degrees.")
                    else Record.Combine({params0,[k=Number.Cos(num("lat_ts",0)*ProjRadians)/Number.Sqrt(1-projectionEll[e2]*Number.Power(Number.Sin(num("lat_ts",0)*ProjRadians),2))]})
                else params0,
            unitName=get("units","m"), unit=if Record.HasFields(raw,"to_meter") then num("to_meter",1)
                else if Record.HasFields(ProjUnits,unitName) then Record.Field(ProjUnits,unitName) else ProjError("Unsupported linear unit."),
            helmert=if Record.HasFields(raw,"towgs84") then List.Transform(Text.Split(raw[towgs84],","),each Number.FromText(_,"en-US"))
                else if datum=null then null else datum[toWGS84],
            checkedHelmert=if helmert=null then null else if not List.Contains({3,7},List.Count(helmert)) or not List.AllTrue(List.Transform(helmert,ProjFinite)) then
                ProjError("+towgs84 requires three or seven finite numbers.") else helmert,
            datumEll=if datum=null then ell else datum[ellipsoid],
            key=if datumName<>null then datumName else "unspecified:" & Number.ToText(ell[a],"G17","en-US") & ":" & Number.ToText(ell[rf],"G17","en-US"),
            grids=get("nadgrids",null),
            result=ProjCreate([Name=if datumName=null then inputMethod else datumName & " / " & inputMethod,
                Method=method, Ellipsoid=projectionEll, DatumEllipsoid=datumEll, DatumKey=key,
                ToWGS84=checkedHelmert, ApproximateDatum=if Record.HasFields(raw,"towgs84") then true else if datum=null then false else datum[approximate],
                Grid=grids, UnitToMeter=unit, parameters=params]),
            axis=get("axis","enu"), pm=get("pm","0"),
            unsupportedParallels=method<>"lcc" and (Record.HasFields(raw,"lat_1") or Record.HasFields(raw,"lat_2")),
            unsupportedZone=inputMethod<>"utm" and (Record.HasFields(raw,"zone") or Record.HasFields(raw,"south"))
        in if axis<>"enu" or pm<>"0" then ProjError("Only GIS east/north axis order and the Greenwich prime meridian are supported.")
            else if get("type","crs")<>"crs" then ProjError("A CRS definition is required, not a PROJ operation pipeline.")
            else if unsupportedParallels or unsupportedZone then ProjError("Projection parameters do not belong to the selected family.")
            else if Record.HasFields(raw,"south") and raw[south]<>"true" then ProjError("+south is a flag; do not supply a value.")
            else result,

    ProjEPSG = [
        #"EPSG:4326"=Record.Combine({ProjFromProj4("+proj=longlat +datum=WGS84"),[Name="WGS 84",Code="EPSG:4326"]}),
        #"EPSG:3857"=Record.Combine({ProjFromProj4("+proj=webmerc +datum=WGS84 +units=m"),[Name="WGS 84 / Pseudo-Mercator",Code="EPSG:3857"]}),
        #"EPSG:27700"=Record.Combine({ProjFromProj4("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m"),[Name="OSGB36 / British National Grid",Code="EPSG:27700"]})
    ],
    ProjResolve = (crs as any) as record =>
        if Value.Is(crs,type text) then
            if Record.HasFields(ProjEPSG,crs) then Record.Field(ProjEPSG,crs)
            else if Text.StartsWith(Text.Trim(crs),"+") then ProjFromProj4(crs)
            else if Text.StartsWith(Text.Trim(crs),"{") then ProjFromJSON(crs)
            else ProjError("Unknown EPSG identifier. Supply a supported PROJ4 or PROJJSON definition.")
        else if Value.Is(crs,type record) and Record.FieldOrDefault(crs,"__CRS__",false) then crs
        else ProjError("Expected a CRS from mgis[proj], or an EPSG/PROJ4/PROJJSON string."),

    ProjJSONUnit = (unit as any, angular as logical) as number =>
        if Value.Is(unit,type record) then unit[conversion_factor]
        else if angular and unit="degree" then ProjRadians
        else if angular and unit="radian" then 1
        else if not angular and List.Contains({"metre","meter"},unit) then 1
        else if not angular and unit="foot" then 0.3048
        else if not angular and unit="US survey foot" then 1200/3937
        else if unit="unity" then 1 else ProjError("Unsupported PROJJSON unit."),
    ProjFromJSON = (json as any) as record =>
        let
            obj=if Value.Is(json,type text) then Json.Document(json) else json,
            geographic=obj[type]="GeographicCRS", base=if geographic then obj else obj[base_crs],
            datum=if Record.HasFields(base,"datum") then base[datum] else base[datum_ensemble],
            ell0=datum[ellipsoid], a=ell0[semi_major_axis],
            rf=if Record.HasFields(ell0,"inverse_flattening") then ell0[inverse_flattening]
                else if ell0[semi_minor_axis]=a then 0 else a/(a-ell0[semi_minor_axis]),
            ell=ProjEllipsoid(a,rf),
            id=Record.FieldOrDefault(datum,"id",null),
            name=datum[name],
            key=if List.Contains({"World Geodetic System 1984","World Geodetic System 1984 ensemble","WGS 84"},name) or (id<>null and id[authority]="EPSG" and List.Contains({6326,"6326"},id[code])) then "WGS84"
                else if name="Ordnance Survey of Great Britain 1936" then "OSGB36"
                else if List.Contains({"North American Datum 1983","NAD83"},name) then "NAD83" else "json:" & name,
            known=if Record.HasFields(ProjDatums,key) then Record.Field(ProjDatums,key) else null,
            conversion=if geographic then null else obj[conversion],
            methodCode=if geographic then "longlat" else Text.From(Record.FieldOrDefault(Record.FieldOrDefault(conversion[method],"id",[]),"code","")),
            methodMap=[#"9807"="tmerc",#"1024"="webmerc",#"9804"="merc",#"9805"="merc",#"9801"="lcc",#"9802"="lcc"],
            method=if geographic then "longlat" else if Record.HasFields(methodMap,methodCode) then Record.Field(methodMap,methodCode)
                else ProjError("Unsupported PROJJSON conversion method. Its EPSG method identifier is required."),
            parameterMap=[#"8801"="lat0",#"8802"="lon0",#"8805"="k",#"8806"="x0",#"8807"="y0",
                #"8821"="lat0",#"8822"="lon0",#"8823"="lat1",#"8824"="lat2",#"8826"="x0",#"8827"="y0"],
            parameters=if geographic then {} else conversion[parameters],
            pairs=List.Transform(parameters,(p)=>let
                code=Text.From(p[id][code]), field=if Record.HasFields(parameterMap,code) then Record.Field(parameterMap,code)
                    else ProjError("Unsupported PROJJSON conversion parameter: " & code),
                angular=List.Contains({"lat0","lon0","lat1","lat2"},field),
                value=p[value]*ProjJSONUnit(p[unit],angular)/(if angular then ProjRadians else 1)
                in [field=field,value=value]),
            params=Record.FromList(List.Transform(pairs,each [value]),List.Transform(pairs,each [field])),
            adjusted=if methodCode="9801" then Record.Combine({params,[lat1=params[lat0],lat2=params[lat0]]})
                else if methodCode="9805" then Record.Combine({params,[k=Number.Cos(params[lat1]*ProjRadians)/Number.Sqrt(1-ell[e2]*Number.Power(Number.Sin(params[lat1]*ProjRadians),2))]}) else params,
            axes=obj[coordinate_system][axis],
            axisUnits=List.Transform(axes,each ProjJSONUnit(_[unit],geographic)),
            prime=Record.FieldOrDefault(base,"prime_meridian",[longitude=0]),
            result=ProjCreate([Name=obj[name],Method=if method="webmerc" then "merc" else method,
                Ellipsoid=if method="webmerc" then ProjEllipsoid(a,0) else ell,
                DatumEllipsoid=ell,DatumKey=key,ToWGS84=if known=null then null else known[toWGS84],
                ApproximateDatum=if known=null then false else known[approximate],Grid=null,
                UnitToMeter=if geographic then 1 else axisUnits{0},parameters=adjusted])
        in if not List.Contains({"GeographicCRS","ProjectedCRS"},obj[type]) then ProjError("Only 2D GeographicCRS and ProjectedCRS PROJJSON definitions are supported.")
            else if List.Count(axes)<>2 or not List.Contains({"ellipsoidal","Cartesian"},obj[coordinate_system][subtype]) then ProjError("Expected a two-dimensional coordinate system.")
            else if not List.ContainsAll(List.Transform(axes,each [direction]),{"east","north"}) then ProjError("Only east/north axes are supported (coordinates are supplied in GIS XY order).")
            else if axisUnits{0}<>axisUnits{1} or (geographic and axisUnits{0}<>ProjRadians) then ProjError("Geographic input must use degrees; projected axes must use the same linear unit.")
            else if prime[longitude]<>0 then ProjError("Only the Greenwich prime meridian is supported.") else result,

    ProjCoordinate = (point as list) as list =>
        if not List.Contains({2,3},List.Count(point)) or not List.AllTrue(List.Transform(point,ProjFinite)) then
            ProjError("Coordinates must contain two or three finite numbers in XY order.") else point,
    ProjInverse = (point as list, crs as record) as list =>
        let
            checked=ProjCoordinate(point), p=crs[parameters],
            xy=if crs[IsGeographic] then {checked{0}*ProjRadians,checked{1}*ProjRadians}
                else {checked{0}*crs[UnitToMeter]-p[x0],checked{1}*crs[UnitToMeter]-p[y0]},
            ll=Record.Field(ProjMethods,crs[Method])[inverse](xy,p,crs[Ellipsoid]),
            h=if List.Count(checked)=3 then checked{2} else 0
        in if not List.AllTrue(List.Transform(ll,ProjFinite)) or Number.Abs(ll{1})>ProjPi/2 then ProjError("Invalid inverse-projected longitude/latitude.")
            else if crs[IsGeographic] and (Number.Abs(checked{0})>180 or Number.Abs(checked{1})>90) then ProjError("Longitude/latitude must be within [-180,180] and [-90,90].")
            else {ll{0},ll{1},h},
    ProjForward = (ll as list, crs as record) as list =>
        let p=crs[parameters], xy=Record.Field(ProjMethods,crs[Method])[forward](ll,p,crs[Ellipsoid]) in
            if not crs[IsGeographic] and Number.Abs(ll{1})>=ProjPi/2 then ProjError("Projected pole coordinates are not supported by this implementation.")
            else if not List.AllTrue(List.Transform(xy,ProjFinite)) then ProjError("Coordinate is outside the projection domain.")
            else if crs[IsGeographic] then {xy{0}/ProjRadians,xy{1}/ProjRadians,ll{2}}
            else {(xy{0}+p[x0])/crs[UnitToMeter],(xy{1}+p[y0])/crs[UnitToMeter],ll{2}},

    ProjGeocentric = (ll as list, ell as record) as list =>
        let n=ell[a]/Number.Sqrt(1-ell[e2]*Number.Power(Number.Sin(ll{1}),2)), r=(n+ll{2})*Number.Cos(ll{1})
        in {r*Number.Cos(ll{0}),r*Number.Sin(ll{0}),(n*(1-ell[e2])+ll{2})*Number.Sin(ll{1})},
    ProjGeodetic = (xyz as list, ell as record) as list =>
        let
            r=Number.Sqrt(xyz{0}*xyz{0}+xyz{1}*xyz{1}),
            phi=if r<1e-10 then Number.Sign(xyz{2})*ProjPi/2 else
                ProjSolve(Number.Atan2(xyz{2},r*(1-ell[e2])),(lat)=>
                    Number.Atan2(xyz{2}+ell[e2]*ell[a]/Number.Sqrt(1-ell[e2]*Number.Power(Number.Sin(lat),2))*Number.Sin(lat),r),1e-13,30),
            n=ell[a]/Number.Sqrt(1-ell[e2]*Number.Power(Number.Sin(phi),2)),
            h=if r<1e-10 then Number.Abs(xyz{2})-ell[b] else r/Number.Cos(phi)-n
        in {Number.Atan2(xyz{1},xyz{0}),phi,h},
    ProjHelmert = (xyz as list, values as list, inverse as logical) as list =>
        let
            v=values & List.Repeat({0},7-List.Count(values)), rx=v{3}*ProjRadians/3600, ry=v{4}*ProjRadians/3600, rz=v{5}*ProjRadians/3600, s=1+v{6}*1e-6,
            x=xyz{0},y=xyz{1},z=xyz{2},
            // Position-vector convention; solve the linear map exactly for inverse.
            tx=(x-v{0})/s,ty=(y-v{1})/s,tz=(z-v{2})/s, det=1+rx*rx+ry*ry+rz*rz
        in if inverse then {
            ((1+rx*rx)*tx+(rz+rx*ry)*ty+(-ry+rx*rz)*tz)/det,
            ((-rz+rx*ry)*tx+(1+ry*ry)*ty+(rx+ry*rz)*tz)/det,
            ((ry+rx*rz)*tx+(-rx+ry*rz)*ty+(1+rz*rz)*tz)/det}
            else {v{0}+s*(x-rz*y+ry*z),v{1}+s*(rz*x+y-rx*z),v{2}+s*(-ry*x+rx*y+z)},
    ProjDatumLeg = (ll as list, crs as record, inverse as logical, options as record) as list =>
        let
            callback=Record.FieldOrDefault(crs,if inverse then "FromWGS84" else "IntoWGS84",null),
            operation=crs[ToWGS84], grid=crs[Grid],
            inputEll=if inverse then ProjEllipsoids[WGS84] else crs[DatumEllipsoid],
            outputEll=if inverse then crs[DatumEllipsoid] else ProjEllipsoids[WGS84],
            allow=Record.FieldOrDefault(options,"allowApproximateDatum",false)
        in if callback<>null then let result=callback(ll) in
                if List.Count(ProjCoordinate(result))<>3 or Number.Abs(result{1})>ProjPi/2 then
                    ProjError("A datum callback must return three finite coordinates with latitude in radians.") else result
            else if grid<>null and grid<>"null" then ProjError("This CRS requires a datum grid: " & grid & ". Supply a pure-M datum transform with proj[withDatumTransform]; grid files are not decoded automatically.")
            else if grid="null" then ll
            else if crs[DatumKey]="WGS84" and operation<>null and List.AllTrue(List.Transform(operation,each _=0)) then ll
            else if operation=null then ProjError("No transformation to WGS84 is defined for datum " & crs[DatumKey] & ".")
            else if crs[ApproximateDatum] and not allow then ProjError("This datum transformation is approximate. Set allowApproximateDatum=true explicitly to use its Helmert parameters.")
            else ProjGeodetic(ProjHelmert(ProjGeocentric(ll,inputEll),operation,inverse),outputEll),
    ProjTransform = (point as list, source as any, target as any, optional options as nullable record) as list =>
        let
            s=ProjResolve(source),t=ProjResolve(target), opts=options ?? [],
            ll=ProjInverse(point,s),
            sameDatum=s[DatumKey]=t[DatumKey] and s[DatumEllipsoid]=t[DatumEllipsoid] and s[ToWGS84]=t[ToWGS84] and s[Grid]=t[Grid]
                and not Record.HasFields(s,"IntoWGS84") and not Record.HasFields(t,"FromWGS84"),
            shifted=if sameDatum then ll else ProjDatumLeg(ProjDatumLeg(ll,s,false,opts),t,true,opts),
            result=ProjForward(shifted,t)
        in List.FirstN(result,List.Count(point)),
    ProjWithDatumTransform = (crs as any, intoWGS84 as function, fromWGS84 as function) as record =>
        Record.Combine({ProjResolve(crs),[IntoWGS84=intoWGS84,FromWGS84=fromWGS84]}),

    // Vincenty's inverse ellipsoidal solution (Survey Review, 1975).
    // A convergence failure is an error, never an unlabelled spherical fallback.
    // Heights are excluded: this is surface distance, not a 3D chord distance.
    ProjGeodesic = (first as list, second as list, ell as record) as number =>
        let
            L=ProjWrap(second{0}-first{0}),
            u1=Number.Atan((1-ell[f])*Number.Tan(first{1})),u2=Number.Atan((1-ell[f])*Number.Tan(second{1})),
            s1=Number.Sin(u1),c1=Number.Cos(u1),s2=Number.Sin(u2),c2=Number.Cos(u2),
            terms=(lambda) => let
                sl=Number.Sin(lambda),cl=Number.Cos(lambda),
                sinSigma=Number.Sqrt(Number.Power(c2*sl,2)+Number.Power(c1*s2-s1*c2*cl,2)),
                cosSigma=s1*s2+c1*c2*cl, sigma=Number.Atan2(sinSigma,cosSigma),
                sinAlpha=if sinSigma<1e-15 then 0 else c1*c2*sl/sinSigma,
                cos2Alpha=1-sinAlpha*sinAlpha,
                cos2SigmaM=if cos2Alpha<1e-15 then 0 else cosSigma-2*s1*s2/cos2Alpha,
                C=ell[f]/16*cos2Alpha*(4+ell[f]*(4-3*cos2Alpha)),
                next=L+(1-C)*ell[f]*sinAlpha*(sigma+C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)))
                in [next=next,sinSigma=sinSigma,cosSigma=cosSigma,sigma=sigma,cos2Alpha=cos2Alpha,cos2SigmaM=cos2SigmaM],
            coincident=Number.Abs(first{1}-second{1})<1e-15 and
                (Number.Abs(L)<1e-15 or Number.Abs(Number.Cos(first{1}))<1e-15),
            solved=try ProjSolve(L,(lambda)=>terms(lambda)[next],1e-12,200),
            v=if solved[HasError] then ProjError("Ellipsoidal geodesic did not converge (nearly antipodal points). No approximate distance has been returned.") else terms(solved[Value]),
            u2sq=v[cos2Alpha]*(ell[a]*ell[a]-ell[b]*ell[b])/(ell[b]*ell[b]),
            A=1+u2sq/16384*(4096+u2sq*(-768+u2sq*(320-175*u2sq))),
            B=u2sq/1024*(256+u2sq*(-128+u2sq*(74-47*u2sq))),
            cm=v[cos2SigmaM], ss=v[sinSigma], cs=v[cosSigma],
            delta=B*ss*(cm+B/4*(cs*(-1+2*cm*cm)-B/6*cm*(-3+4*ss*ss)*(-3+4*cm*cm))),
            distance=ell[b]*A*(v[sigma]-delta)
        in if ell[f]>0.02 then ProjError("The geodesic series requires flattening <= 1/50.")
            else if coincident then 0 else distance,
    ProjDistance = (first as list, second as list, crs as any, optional options as nullable record) as number =>
        let
            c=ProjResolve(crs),opts=options ?? [],mode=Record.FieldOrDefault(opts,"mode","Geodesic"),
            analysis=ProjResolve(Record.FieldOrDefault(opts,"analysisCRS",c)),
            p=ProjTransform(first,c,analysis,opts),q=ProjTransform(second,c,analysis,opts)
        in if mode="Geodesic" then ProjGeodesic(ProjInverse(first,c),ProjInverse(second,c),c[DatumEllipsoid])
            else if mode<>"Planar" then ProjError("Distance mode must be Planar or Geodesic.")
            else if analysis[IsGeographic] then ProjError("Planar distances in metres require a projected analysis CRS.")
            else Number.Sqrt(Number.Power(p{0}-q{0},2)+Number.Power(p{1}-q{1},2))*analysis[UnitToMeter],

    // Transform vertices recursively, preserving all non-coordinate fields.
    GeometryReproject = (geometry as record, source as any, target as any, options as record) as record =>
        if geometry[Kind]="POINT" then let
            point=ProjTransform({geometry[X],geometry[Y]},source,target,options)
            in Record.TransformFields(geometry,{{"X",each point{0}},{"Y",each point{1}}})
        else let
            field=if geometry[Kind]="LINESTRING" then "Points" else if geometry[Kind]="POLYGON" then "Rings" else "Components"
            in Record.TransformFields(geometry,{{field,each List.Transform(_,(g)=>@GeometryReproject(g,source,target,options))}}),
    ShapeReproject = (shape as record, source as any, target as any, optional options as nullable record) as record =>
        let g=GeometryReproject(shape[Geometry],source,target,options ?? []) in
            Record.TransformFields(shape,{{"Geometry",each g},{"Envelope",each GeometryGetEnvelope(g)}}),

    // RFC 7946 coordinate arrays use longitude/latitude order. An explicitly
    // supplied layer CRS can describe non-standard projected GeoJSON input.
    GeometryFromGeoJSON = (geometry as record) as record =>
        let
            point=(xy)=>let checked=ProjCoordinate(xy) in [Kind="POINT",X=checked{0},Y=checked{1}],
            line=(coordinates)=>if List.Count(coordinates)<2 then ProjError("GeoJSON lines require at least two coordinates.")
                else [Kind="LINESTRING",Points=List.Transform(coordinates,point)],
            polygon=(coordinates)=>if List.IsEmpty(coordinates) then ProjError("Empty GeoJSON polygons are not supported.") else
                [Kind="POLYGON",Rings=List.Transform(coordinates,(ring)=>
                    if List.Count(ring)<4 or List.FirstN(ring{0},2)<>List.FirstN(List.Last(ring),2) then
                        ProjError("GeoJSON polygon rings require at least four coordinates and must be closed.") else line(ring))],
            kind=Text.Upper(geometry[type]),coordinates=Record.FieldOrDefault(geometry,"coordinates",null)
        in if kind="GEOMETRYCOLLECTION" and List.IsEmpty(geometry[geometries]) then ProjError("Empty GeoJSON geometries are not supported.")
            else if kind<>"GEOMETRYCOLLECTION" and (coordinates=null or List.IsEmpty(coordinates)) then ProjError("Empty GeoJSON geometries are not supported.")
            else if kind="POINT" then point(coordinates)
            else if kind="LINESTRING" then line(coordinates)
            else if kind="POLYGON" then polygon(coordinates)
            else if kind="MULTIPOINT" then [Kind=kind,Components=List.Transform(coordinates,point)]
            else if kind="MULTILINESTRING" then [Kind=kind,Components=List.Transform(coordinates,line)]
            else if kind="MULTIPOLYGON" then [Kind=kind,Components=List.Transform(coordinates,polygon)]
            else if kind="GEOMETRYCOLLECTION" then [Kind=kind,Components=List.Transform(geometry[geometries],each @GeometryFromGeoJSON(_))]
            else ProjError("Unsupported GeoJSON geometry type."),
    ShapeCreateFromGeoJSON = (json as any) as record =>
        let
            obj=if Value.Is(json,type text) then Json.Document(json) else json,
            g=GeometryFromGeoJSON(if obj[type]="Feature" then obj[geometry] else obj)
        in [__TShapeIdentifier__=null,Kind=g[Kind],Geometry=g,Envelope=GeometryGetEnvelope(g),__rowid__=null],
    
    //Creates a TShape point from latitude and longitude coordinates
    //@param lat - The latitude coordinate
    //@param lng - The longitude coordinate
    //@returns - A TShape record representing the point geometry
    ShapeCreatePointFromLatLng = Value.ReplaceType(
        (lat as number, lng as number) as record => (
            let
                baseGeometry = GeometryPoint.From(lng, lat)
            in [
                __TShapeIdentifier__ = null,
                Kind = baseGeometry[Kind],
                Geometry = baseGeometry,
                Envelope = GeometryGetEnvelope(baseGeometry),
                __rowid__ = null
            ]
        ),
        type function (lat as number, lng as number) as TShape
    ),
    //Creates a TShape from Well-Known Text (WKT) representation
    //@param wkt - The Well-Known Text string representing the geometry
    //@returns - A TShape record representing the geometry
    ShapeCreateFromWKT = Value.ReplaceType(
        (wkt as text) as record => (
            let 
                baseGeometry = Geometry.FromWellKnownText(wkt),
                envelope = GeometryGetEnvelope(baseGeometry)
            in [
                __TShapeIdentifier__ = null,
                Kind = baseGeometry[Kind],
                Geometry = baseGeometry,
                Envelope = envelope,
                __rowid__ = null
            ] 
        ), 
        type function (wkt as text) as TShape
    ),
    //Ensures a table has a numeric __rowid__ column unique per row
    //@param tbl - The table to ensure has a __rowid__ column
    //@returns - The table with __rowid__ column (added as index starting from 0 if not present)
    EnsureRowIdColumn = (tbl as table) as table => (
        let
            hasCol = Table.HasColumns(tbl, "__rowid__"),
            withId = if hasCol then tbl else Table.AddIndexColumn(tbl, "__rowid__", 0, 1, Int64.Type)
        in
            withId
    ),

    //**************************************************
    // Quadtree Implementation
    //**************************************************
    TQuadTreeNode = type [
        //Shapes is where the actual shapes are stored. Sometimes a polygon or line may overlap multiple leaves,
        //we store these in the parent node's children list.
        shapes = {TShape},
        
        //The capacity of a node before it is split into children.
        capacity = number,

        //Children is where we store any children nodes/leaves. All leaves are nodes (without `children`)
        children = nullable {@TQuadTreeNode},

        //The envelope of the node.
        envelope = TShapeEnvelope
    ],
    TQuadTree = type [
        //The top level quadtree node.
        root = TQuadTreeNode,
        
        //The capacity of a node before it is split into children.
        capacity = number
    ],
    TLayer = type [
        table = table,
        geometryColumn = text,
        queryLayer = TQuadTree,
        TProjection = nullable TProjection
    ],
    TQuadTreeQueryOperator = type [
        // Required: per‑candidate callback
        onCandidate = function (candidate as TShape, query as TShape) as any,

        // Optional: aggregate child/node results
        combine = nullable function (childResults as list) as any,

        // Optional: early‑exit / pruning rule
        continueSearch = nullable function (currentBest as any) as logical,

        // Optional: additional columns that should be present in results
        additionalColumns = nullable {text}
    ],
    TRowQueryOperator = type function (candidate as record) as logical,
    //Checks if two bounding boxes (envelopes) intersect
    //@param b1 - The first bounding box
    //@param b2 - The second bounding box
    //@returns - True if the boxes intersect, false otherwise
    QuadTreeBoxesIntersect = Value.ReplaceType(
        (b1 as record, b2 as record) as logical => (
            not (
                b1[MaxX] < b2[MinX] or
                b1[MinX] > b2[MaxX] or
                b1[MaxY] < b2[MinY] or
                b1[MinY] > b2[MaxY]
            )
        ),
        type function (b1 as TShapeEnvelope, b2 as TShapeEnvelope) as logical
    ),

    // Cache the bounds of all envelope centres below a node. These remain valid
    // even when root expansion changes the tree's subdivision envelopes.
    QuadTreeUpdateNearestEnvelope = (node as record) as record =>
        let
            ownCentres = List.Transform(node[shapes], (shape) =>
                let
                    e = shape[Envelope],
                    x = (e[MinX] + e[MaxX]) / 2,
                    y = (e[MinY] + e[MaxY]) / 2
                in
                    [MinX = x, MinY = y, MaxX = x, MaxY = y]
            ),
            childBounds = if node[children] = null then {} else
                List.Transform(node[children], each _[nearestEnvelope]),
            bounds = List.RemoveNulls(ownCentres & childBounds),
            envelope = if List.IsEmpty(bounds) then null else [
                MinX = List.Min(List.Transform(bounds, each [MinX])),
                MinY = List.Min(List.Transform(bounds, each [MinY])),
                MaxX = List.Max(List.Transform(bounds, each [MaxX])),
                MaxY = List.Max(List.Transform(bounds, each [MaxY]))
            ]
        in
            Record.Combine({node, [nearestEnvelope = envelope]}),
    //Subdivides a quadtree node into four children (SW, SE, NE, NW)
    //@param node - The node to subdivide
    //@returns - The subdivided node with four children
    QuadTreeSubdivideNode = Value.ReplaceType(
        (node as record) as record => (
            let
                midX = (node[envelope][MinX] + node[envelope][MaxX]) / 2,
                midY = (node[envelope][MinY] + node[envelope][MaxY]) / 2,
                b = node[envelope],
                children = {
                    // SW
                    [envelope = [MinX = b[MinX], MinY = b[MinY], MaxX = midX, MaxY = midY], capacity = node[capacity], shapes = {}, children = null, nearestEnvelope = null],
                    // SE
                    [envelope = [MinX = midX, MinY = b[MinY], MaxX = b[MaxX], MaxY = midY], capacity = node[capacity], shapes = {}, children = null, nearestEnvelope = null],
                    // NE
                    [envelope = [MinX = midX, MinY = midY, MaxX = b[MaxX], MaxY = b[MaxY]], capacity = node[capacity], shapes = {}, children = null, nearestEnvelope = null],
                    // NW
                    [envelope = [MinX = b[MinX], MinY = midY, MaxX = midX, MaxY = b[MaxY]], capacity = node[capacity], shapes = {}, children = null, nearestEnvelope = null]
                }
            in                
                if Record.HasFields(node, {"children"}) then
                    Record.TransformFields(node, {"children", each children})
                else
                    Record.AddField(node, "children", children)
        ),
        type function (node as TQuadTreeNode) as TQuadTreeNode
    ),
    //Creates a new empty quadtree with specified capacity
    //@param capacity - The maximum number of shapes per node before subdivision
    //@param initialEnvelope - The initial bounding envelope (null uses default [0,0,0,0])
    //@returns - A new empty quadtree
    QuadTreeCreate = Value.ReplaceType(
        (capacity as number, initialEnvelope as nullable record) as record => (
            [
                root = [
                    shapes = {},
                    capacity = capacity,
                    children = null,
                    nearestEnvelope = null,
                    envelope = initialEnvelope ?? [
                        MinX = 0,
                        MinY = 0,
                        MaxX = 0,
                        MaxY = 0
                    ]
                ],
                capacity = capacity
            ]
        ),
        type function (capacity as number, initialEnvelope as nullable TShapeEnvelope) as TQuadTree
    ),
    //Inserts a shape into a quadtree node, handling subdivision and envelope expansion as needed
    //@param node - The node to insert the shape into
    //@param shape - The shape to insert
    //@param isRoot - Whether this node is the root of the quadtree
    //@returns - The updated node with the shape inserted
    QuadTreeNodeInsert = Value.ReplaceType(
        (node as record, shape as record, isRoot as logical) as record => (
            let
                _ = if not Record.HasFields(shape, {"__TShapeIdentifier__"}) then error "Shape is not a TShape" else null,
                bNode = node[envelope],
                bShape = shape[Envelope],
                intersects = QuadTreeBoxesIntersect(bNode, bShape),

                // Inserts the shape into the single child it intersects; otherwise keeps it on this node.
                // @param state - The quadtree node being updated (its children/shapes may change)
                // @param s - The shape to insert or retain at this node
                insertIntoChildrenOrKeep = (state as record, s as record) as record => (
                    let
                        bbox = s[Envelope],
                        children = state[children],
                        containedIndexes = List.Select(
                            List.Positions(children),
                            (i as number) as logical => QuadTreeBoxesIntersect(bbox, children{i}[envelope])
                        )
                    in
                        if List.Count(containedIndexes) = 1 then
                            let
                                idx = List.First(containedIndexes),
                                updatedChildren = List.Transform(
                                    {0..List.Count(children)-1},
                                    (i as number) as record => if i = idx then @QuadTreeNodeInsert(children{i}, s, false) else children{i}
                                )
                            in
                                Record.TransformFields(state, {"children", each updatedChildren})
                        else
                            Record.TransformFields(state, {"shapes", each _ & {s}})
                ),
                expandEnvelope = (p as record, t as record) as record => [
                    MinX = List.Min({p[MinX], t[MinX]}),
                    MinY = List.Min({p[MinY], t[MinY]}),
                    MaxX = List.Max({p[MaxX], t[MaxX]}),
                    MaxY = List.Max({p[MaxY], t[MaxY]})
                ],
                createExpandedParent = (node as record, shape as record) as record => (
                    let
                        b1 = node[envelope],
                        b2 = shape[Envelope],
						// Build a square parent envelope starting from the existing node,
						// then double its size and shift until it fully contains the shape's envelope
						w1 = b1[MaxX] - b1[MinX],
						h1 = b1[MaxY] - b1[MinY],
						baseSize = if w1 > h1 then w1 else h1,
						size0 = if baseSize = 0 then 1 else baseSize,
						parent0 = [
							MinX = b1[MinX],
							MinY = b1[MinY],
							MaxX = b1[MinX] + size0,
							MaxY = b1[MinY] + size0
						],
						contains = (p as record, t as record) as logical =>
							(t[MinX] >= p[MinX]) and (t[MaxX] <= p[MaxX]) and (t[MinY] >= p[MinY]) and (t[MaxY] <= p[MaxY]),
						expand = (p as record, size as number) as record =>
							if @contains(p, b2) then p else
							let
								dx = if b2[MinX] < p[MinX] then -1 else if b2[MaxX] > p[MaxX] then 1 else 0,
								dy = if b2[MinY] < p[MinY] then -1 else if b2[MaxY] > p[MaxY] then 1 else 0,
								p2 = [
									MinX = p[MinX] - (if dx = -1 then size else 0),
									MinY = p[MinY] - (if dy = -1 then size else 0),
									MaxX = p[MaxX] + (if dx = 1 then size else 0),
									MaxY = p[MaxY] + (if dy = 1 then size else 0)
								]
							in
								@expand(p2, size * 2),
						parentEnvelope = expand(parent0, size0),
						midX = (parentEnvelope[MinX] + parentEnvelope[MaxX]) / 2,
						midY = (parentEnvelope[MinY] + parentEnvelope[MaxY]) / 2,
						quadrants = {
							[MinX = parentEnvelope[MinX], MinY = parentEnvelope[MinY], MaxX = midX, MaxY = midY], // SW
							[MinX = midX, MinY = parentEnvelope[MinY], MaxX = parentEnvelope[MaxX], MaxY = midY], // SE
							[MinX = midX, MinY = midY, MaxX = parentEnvelope[MaxX], MaxY = parentEnvelope[MaxY]], // NE
							[MinX = parentEnvelope[MinX], MinY = midY, MaxX = midX, MaxY = parentEnvelope[MaxY]]  // NW
						},
						cx = (b1[MinX] + b1[MaxX]) / 2,
						cy = (b1[MinY] + b1[MaxY]) / 2,
						qx = if cx < midX then 0 else 1,
						qy = if cy < midY then 0 else 1,
						idx = if qx = 0 and qy = 0 then 0 else if qx = 1 and qy = 0 then 1 else if qx = 1 and qy = 1 then 2 else 3,
						makeChild = (env as record) as record => [envelope = env, capacity = node[capacity], shapes = {}, children = null, nearestEnvelope = null],
						baseChildren = List.Transform(quadrants, each @makeChild(_)),
						updatedChildren = List.Transform({0..3}, (i as number) as record => if i = idx then Record.TransformFields(node, {"envelope", each quadrants{i}}) else baseChildren{i}),
						newParent = [
							envelope = parentEnvelope,
							capacity = node[capacity],
							shapes = {},
							children = updatedChildren
						],
						withShape = insertIntoChildrenOrKeep(newParent, shape)
					in
						withShape
				),
                result =
                    //If the shape does not intersect the node, return the node unchanged
                    if not intersects then
                        //If the node is the root, then we need to expand the envelope to include the shape either by creating a new parent (if the capacity is exceeded) or expanding the existing one, then insert
                        if isRoot then
                            if List.Count(node[shapes]) < node[capacity] then
                                let
                                    expanded = Record.TransformFields(node, {"envelope", each @expandEnvelope(_, bShape)}),
                                    reinserted = @QuadTreeNodeInsert(expanded, shape, true)
                                in
                                    reinserted
                            else
                                //Create a new parent and expand the envelope to include the shape
                                createExpandedParent(node, shape)
                        else
                            node
                    //If the node has no children
                    else if node[children] = null then
                        //If the node has capacity, add the shape to the node
                        if List.Count(node[shapes]) < node[capacity]
                            or List.AllTrue(List.Transform(node[shapes], each _[Envelope] = bShape)) then
                            Record.TransformFields(node, {"shapes", each _ & {shape}})
                        //If the node does not have capacity, subdivide the node and redistribute the shapes
                        else
                            let
                                subdivided = QuadTreeSubdivideNode(node),
                                allShapes = node[shapes] & {shape},
                                // Move existing shapes into children without retaining
                                // duplicate copies on the subdivided parent.
                                cleared = Record.TransformFields(subdivided, {"shapes", each {}}),
                                reshaped = List.Accumulate(allShapes, cleared, insertIntoChildrenOrKeep)
                            in
                                reshaped
                    //If the node has children
                    else
                        //Insert the shape into the appropriate child
                        insertIntoChildrenOrKeep(node, shape)
            in
                QuadTreeUpdateNearestEnvelope(result)
        ),
        type function (node as TQuadTreeNode, shape as TShape, isRoot as logical) as TQuadTreeNode
    ),
    
    //Inserts a shape into a quadtree
    //@param quadTree - The quadtree to insert the shape into
    //@param shape - The shape to insert
    //@returns - The updated quadtree with the shape inserted
    QuadTreeInsert = Value.ReplaceType(
        (quadTree as record, shape as record) as record => (
            let
                _ = if not Record.HasFields(shape, {"__TShapeIdentifier__"}) then error "Shape is not a TShape" else null,
                result = QuadTreeNodeInsert(quadTree[root], shape, true),
                updatedRoot = Record.TransformFields(quadTree, {"root", each result})
            in
                updatedRoot
        ),
        type function (quadTree as TQuadTree, shape as TShape) as TQuadTree
    ),

    // Bulk loading avoids a long lazy chain of inserts and repeated root
    // expansion when building/reprojecting a whole table.
    QuadTreeBuild = (shapes as list, capacity as number) as record =>
        let
            items=List.Buffer(shapes), envelopes=List.Transform(items,each [Envelope]),
            bounds=if List.IsEmpty(items) then [MinX=0,MinY=0,MaxX=0,MaxY=0] else [
                MinX=List.Min(List.Transform(envelopes,each [MinX])),MinY=List.Min(List.Transform(envelopes,each [MinY])),
                MaxX=List.Max(List.Transform(envelopes,each [MaxX])),MaxY=List.Max(List.Transform(envelopes,each [MaxY]))],
            build=(members as list, envelope as record) as record => let
                buffered=List.Buffer(members),
                mx=(envelope[MinX]+envelope[MaxX])/2,my=(envelope[MinY]+envelope[MaxY])/2,
                leaf=List.Count(buffered)<=capacity or
                    List.AllTrue(List.Transform(buffered,each [Envelope]=buffered{0}[Envelope])) or
                    ((mx=envelope[MinX] or mx=envelope[MaxX]) and (my=envelope[MinY] or my=envelope[MaxY])),
                quadrants={
                    [MinX=envelope[MinX],MinY=envelope[MinY],MaxX=mx,MaxY=my],
                    [MinX=mx,MinY=envelope[MinY],MaxX=envelope[MaxX],MaxY=my],
                    [MinX=mx,MinY=my,MaxX=envelope[MaxX],MaxY=envelope[MaxY]],
                    [MinX=envelope[MinX],MinY=my,MaxX=mx,MaxY=envelope[MaxY]]},
                assigned=List.Buffer(List.Transform(buffered,(shape)=>let
                    indexes=List.Select({0..3},(i)=>QuadTreeBoxesIntersect(shape[Envelope],quadrants{i}))
                    in [shape=shape,child=if List.Count(indexes)=1 then indexes{0} else -1])),
                own=if leaf then buffered else List.Transform(List.Select(assigned,each [child]=-1),each [shape]),
                children=if leaf or List.Count(own)=List.Count(buffered) then null else
                    List.Buffer(List.Transform({0..3},(i)=>@build(List.Transform(List.Select(assigned,each [child]=i),each [shape]),quadrants{i}))),
                node=QuadTreeUpdateNearestEnvelope([envelope=envelope,capacity=capacity,shapes=own,children=children]),
                nearest=node[nearestEnvelope]
                // Force the four cached scalars while this node is built.
                in if nearest=null then node else if List.Count(List.Buffer(Record.FieldValues(nearest)))=4 then node else ProjError("Invalid index bounds."),
            root=build(items,bounds)
        in [root=root,capacity=capacity],



    //Query operator that tests if candidate envelope intersects query envelope
    //@returns - An operator that returns shapes whose envelopes intersect the query shape
    //@remark This is an envelope-based test, not a true geometric intersection
    QuadTreeOperatorEnvelopeIntersects = Value.ReplaceType([
        onCandidate = (candidate as record, query as record) as list =>
            let
                _ = if not Record.HasFields(candidate, {"__TShapeIdentifier__"}) then error "Candidate is not a TShape" else null,
                _2 = if not Record.HasFields(query, {"__TShapeIdentifier__"}) then error "Query is not a TShape" else null,
                a = candidate[Envelope],
                b = query[Envelope],
                intersects =
                    not (
                        a[MaxX] < b[MinX] or
                        a[MinX] > b[MaxX] or
                        a[MaxY] < b[MinY] or
                        a[MinY] > b[MaxY]
                    )
            in
                if intersects then { [shape=candidate] } else {},
            combine = null,
            continueSearch = null,
            additionalColumns = null
        ],
        TQuadTreeQueryOperator
    ),
    
    //Query operator that tests if candidate geometry truly intersects query geometry
    //@returns - An operator that returns shapes that geometrically intersect the query shape
    //@remark This performs true geometric intersection checking all vertices, with envelope pre-filter for performance
    QuadTreeOperatorIntersects = Value.ReplaceType([
        onCandidate = (candidate as record, query as record) as list =>
            let
                //Use envelope operator as fast pre-filter (includes validation)
                envelopeResult = QuadTreeOperatorEnvelopeIntersects[onCandidate](candidate, query),
                //Only do expensive geometric check if envelope test passed
                intersects = if List.Count(envelopeResult) > 0 then
                    GeometryIntersects(candidate[Geometry], query[Geometry])
                else
                    false
            in
                if intersects then envelopeResult else {},
            combine = null,
            continueSearch = null,
            additionalColumns = null
        ],
        TQuadTreeQueryOperator
    ),

    //Query operator that tests if candidate envelope fully contains query envelope
    //@returns - An operator that returns shapes whose envelopes contain the query shape
    //@remark This is an envelope-based test, not a true geometric containment
    QuadTreeOperatorEnvelopeContains = Value.ReplaceType(
        [
            onCandidate = (candidate as record, query as record) as list =>
                let
                    _ = if not Record.HasFields(candidate, {"__TShapeIdentifier__"}) then error "Candidate is not a TShape" else null,
                    _2 = if not Record.HasFields(query, {"__TShapeIdentifier__"}) then error "Query is not a TShape" else null,
                    p = candidate[Envelope],
                    t = query[Envelope],
                    contains =
                        (t[MinX] >= p[MinX]) and (t[MaxX] <= p[MaxX]) and
                        (t[MinY] >= p[MinY]) and (t[MaxY] <= p[MaxY])
                in
                    if contains then { [shape=candidate] } else {},
            combine = null,
            continueSearch = null,
            additionalColumns = null
        ],
        TQuadTreeQueryOperator
    ),
    
    //Query operator that tests if candidate geometry truly contains query geometry
    //@returns - An operator that returns shapes that geometrically contain the query shape
    //@remark This performs true geometric containment checking all vertices, with envelope pre-filter for performance
    QuadTreeOperatorContains = Value.ReplaceType([
        onCandidate = (candidate as record, query as record) as list =>
            let
                //Use envelope operator as fast pre-filter (includes validation)
                envelopeResult = QuadTreeOperatorEnvelopeContains[onCandidate](candidate, query),
                //Only do expensive geometric check if envelope test passed
                contains = if List.Count(envelopeResult) > 0 then
                    GeometryContains(candidate[Geometry], query[Geometry])
                else
                    false
            in
                if contains then envelopeResult else {},
            combine = null,
            continueSearch = null,
            additionalColumns = null
        ],
        TQuadTreeQueryOperator
    ),
    
    //Query operator that tests if candidate envelope is fully inside query envelope
	//@returns - An operator that returns shapes whose envelopes are within the query shape
	//@remark This is an envelope-based test, not a true geometric within relationship
	QuadTreeOperatorEnvelopeWithin = Value.ReplaceType(
	    [
	        onCandidate = (candidate as record, query as record) as list =>
	            let
                    _ = if not Record.HasFields(candidate, {"__TShapeIdentifier__"}) then error "Candidate is not a TShape" else null,
                    _2 = if not Record.HasFields(query, {"__TShapeIdentifier__"}) then error "Query is not a TShape" else null,
	                p = query[Envelope],
	                t = candidate[Envelope],
	                within =
	                    (t[MinX] >= p[MinX]) and (t[MaxX] <= p[MaxX]) and
	                    (t[MinY] >= p[MinY]) and (t[MaxY] <= p[MaxY])
	            in
	                if within then { [shape=candidate] } else {},
	        combine = null,
	        continueSearch = null,
	        additionalColumns = null
	    ],
	    TQuadTreeQueryOperator
	),
    
    //Query operator that tests if candidate geometry is truly within query geometry
    //@returns - An operator that returns shapes that are geometrically within the query shape
    //@remark This performs true geometric within checking all vertices, with envelope pre-filter for performance
    QuadTreeOperatorWithin = Value.ReplaceType([
        onCandidate = (candidate as record, query as record) as list =>
            let
                //Use envelope operator as fast pre-filter (includes validation)
                envelopeResult = QuadTreeOperatorEnvelopeWithin[onCandidate](candidate, query),
                //Only do expensive geometric check if envelope test passed
                within = if List.Count(envelopeResult) > 0 then
                    GeometryWithin(candidate[Geometry], query[Geometry])
                else
                    false
            in
                if within then envelopeResult else {},
            combine = null,
            continueSearch = null,
            additionalColumns = null
        ],
        TQuadTreeQueryOperator
    ),

    //Query operator that finds the k nearest neighbors to the query shape
    //@param k - The number of nearest neighbors to find
    //@returns - An operator that returns the k nearest shapes based on envelope center distance
    //@remark Distance is calculated between envelope centers, not actual geometry. Results include a 'dist' column.
    QuadTreeOperatorNearestN = Value.ReplaceType(
        (k as number) as record =>
            if not ProjFinite(k) or k<0 or Number.RoundDown(k)<>k then
                ProjError("Nearest-neighbour count must be a finite non-negative integer.") else
            [
                onCandidate = (candidate as record, query as record) as record =>
                    let
                        _ = if not Record.HasFields(candidate, {"__TShapeIdentifier__"}) then error "Candidate is not a TShape" else null,
                        _2 = if not Record.HasFields(query, {"__TShapeIdentifier__"}) then error "Query is not a TShape" else null,
                        e1  = candidate[Envelope],
                        e2  = query[Envelope],
                        cx1 = (e1[MinX] + e1[MaxX]) / 2,
                        cy1 = (e1[MinY] + e1[MaxY]) / 2,
                        cx2 = (e2[MinX] + e2[MaxX]) / 2,
                        cy2 = (e2[MinY] + e2[MaxY]) / 2,
                        dist = Number.Sqrt((cx1 - cx2)*(cx1 - cx2) + (cy1 - cy2)*(cy1 - cy2))
                    in
                        [ shape = candidate, dist = dist ],

                combine = (lists as list) as list =>
                    let
                        all   = List.Combine(lists),
                        sorted = List.Sort(all, each _[dist]),
                        bestN  = List.FirstN(sorted, k)
                    in
                        bestN,

                // Having k candidates does not mean they are the closest k.
                continueSearch = null,

                additionalColumns = {"dist"},
                // Extra traversal metadata; kept out of the record type because
                // Value.ReplaceType requires the existing operators' exact fields.
                nearestCount = k
            ],
            type function (k as number) as TQuadTreeQueryOperator
    ),

    //Query operator that finds the single nearest neighbor to the query shape
    //@returns TQuadTreeQueryOperator - An operator that returns the nearest shape based on envelope center distance
    //@remark This is a convenience wrapper for QuadTreeOperatorNearestN(1)
    QuadTreeOperatorNearest = QuadTreeOperatorNearestN(1),

    QuadTreeOperatorNearestGeodesicN = (k as number) as record =>
        Record.Combine({QuadTreeOperatorNearestN(k),[
            distanceMode="Geodesic",
            onCandidate=(candidate as record, query as record) as record =>
                if candidate[Kind]<>"POINT" or query[Kind]<>"POINT" then
                    ProjError("Geodesic nearest-neighbour queries currently require point geometries.")
                else Record.FromList(List.Buffer({candidate,ProjDistance(
                    {candidate[Geometry][X],candidate[Geometry][Y]},
                    {query[Geometry][X],query[Geometry][Y]},ProjEPSG[#"EPSG:4326"])}),{"shape","dist"})
        ]}),
    ProjUnitVector = (longitude as number, latitude as number) as list =>
        let lon=longitude*ProjRadians,lat=latitude*ProjRadians,c=Number.Cos(lat)
        in {c*Number.Cos(lon),c*Number.Sin(lon),Number.Sin(lat)},
    // Cache 3D unit-sphere bounds ONCE per analysis index, not once per query.
    // These bounds handle poles and the date line without longitude wrapping.
    QuadTreePrepareGeodesic = (node as record) as record =>
        let
            children=if node[children]=null then null else List.Buffer(List.Transform(node[children],each @QuadTreePrepareGeodesic(_))),
            own=List.Buffer(List.Transform(node[shapes],each
                if _[Kind]<>"POINT" then ProjError("Geodesic nearest-neighbour queries require point geometries.")
                else let v=List.Buffer(ProjUnitVector(_[Geometry][X],_[Geometry][Y])) in
                    if v{0}<=1 then [min=v,max=v] else ProjError("Invalid geodesic index coordinate."))),
            childBounds=if children=null then {} else List.Buffer(List.RemoveNulls(List.Transform(children,each [geodesicBounds]))),
            all=List.Buffer(own & childBounds),
            bounds=if List.IsEmpty(all) then null else [
                min=List.Buffer(List.Transform({0..2},(i)=>List.Min(List.Transform(all,each [min]{i})))),
                max=List.Buffer(List.Transform({0..2},(i)=>List.Max(List.Transform(all,each [max]{i}))))
            ]
        in if bounds=null then Record.Combine({node,[children=children,geodesicBounds=null]})
            else if bounds[min]{0}<=bounds[max]{0} then Record.Combine({node,[children=children,geodesicBounds=bounds]})
            else ProjError("Invalid geodesic index bounds."),

    //Normalizes a value to a list, converting null to empty list and single values to single-element lists
    //@param value as (Null | List<Any> | Any) - The value to normalize to a list
    //@returns List<any> - A list representation of the input value
    NormalizeList = (value as any) as list =>
        if value = null then
            {}
        else if Value.Is(value, type list) then
            value
        else
            { value },

    //Ensures all records have the same schema by adding missing columns as null
    //@param records as List<Record> - The list of records to normalize
    //@param additionalColumns as (Null | List<Text>) - Additional column names that should be present in all records
    //@returns List<Record> - The normalized list of records with consistent schema
    NormalizeRecords = (records as list, additionalColumns as nullable list) as list =>
        if additionalColumns = null or List.Count(additionalColumns) = 0 then
            records
        else
            List.Transform(
                records,
                (r as record) as record =>
                    let
                        existingFields = Record.FieldNames(r),
                        missingFields = List.RemoveItems(additionalColumns, existingFields),
                        nullFields = List.Transform(missingFields, each [Name = _, Value = null]),
                        additionalRecord = Record.FromList(List.Transform(nullFields, each _[Value]), List.Transform(nullFields, each _[Name]))
                    in
                        Record.Combine({r, additionalRecord})
            ),

    //Queries a quadtree node based on a spatial relationship to a supplied shape
    //@param node - The node to query
    //@param shape - The shape to query against
    //@param operator - The query operator defining the spatial relationship
    //@returns (List<Record> | Any) - Query results (type depends on operator's combine function, typically List<Record>)
    QuadTreeNodeQuery = Value.ReplaceType(
        (node as record, shape as record, operator as record) as any =>
            let
                _ = if not Record.HasFields(shape, {"__TShapeIdentifier__"})
                        then error "Shape is not a TShape"
                        else null,

                onCandidate    = operator[onCandidate],
                combine        = if operator[combine] <> null
                                    then operator[combine]
                                    else List.Combine,
                continueSearch = if operator[continueSearch] <> null
                                    then operator[continueSearch]
                                    else (x as any) => true,

                queryEnvelope = shape[Envelope],
                nodeEnvelope  = node[envelope],
                nodeIntersects = QuadTreeBoxesIntersect(nodeEnvelope, queryEnvelope),

                //------------------------------------------
                // Candidate results at this node
                //------------------------------------------
                fromNodeRaw =
                    if nodeIntersects
                        then List.Transform(node[shapes], each onCandidate(_, shape))
                        else {},
                fromNode = NormalizeList(List.Combine(List.Transform(fromNodeRaw, each NormalizeList(_)))),

                //------------------------------------------
                // Recurse into children
                //------------------------------------------
                fromChildrenRaw =
                    if nodeIntersects and node[children] <> null and continueSearch(fromNode)
                        then List.Transform(
                                node[children],
                                (c as record) =>
                                    if QuadTreeBoxesIntersect(c[envelope], queryEnvelope)
                                        then @QuadTreeNodeQuery(c, shape, operator)
                                        else {})
                        else {},
                fromChildren = NormalizeList(List.Combine(List.Transform(fromChildrenRaw, each NormalizeList(_)))),

                //------------------------------------------
                // Combine results safely
                //------------------------------------------
                result = combine({ fromNode, fromChildren })
            in
                result,

        type function (node as TQuadTreeNode, shape as TShape, operator as TQuadTreeQueryOperator) as any
    ),

    // Branch-and-bound nearest search. Visit the closest child first, carrying
    // the best k candidates forward so farther subtrees can be skipped entirely.
    QuadTreeQueryNearest = (node as record, shape as record, operator as record) as list =>
        let
            k = operator[nearestCount],
            e = shape[Envelope],
            x = (e[MinX] + e[MaxX]) / 2,
            y = (e[MinY] + e[MaxY]) / 2,
            geodesic=Record.FieldOrDefault(operator,"distanceMode",null)="Geodesic",
            vector=ProjUnitVector(x,y),
            minimumDistance = (current as record) as nullable number =>
                let
                    bounds = Record.FieldOrDefault(current, "nearestEnvelope", current[envelope]),
                    dx = if bounds = null then 0 else List.Max({bounds[MinX] - x, 0, x - bounds[MaxX]}),
                    dy = if bounds = null then 0 else List.Max({bounds[MinY] - y, 0, y - bounds[MaxY]}),
                    sphereBounds=Record.FieldOrDefault(current,"geodesicBounds",null),
                    gaps=if sphereBounds=null then {} else List.Transform({0..2},(i)=>List.Max({sphereBounds[min]{i}-vector{i},0,vector{i}-sphereBounds[max]{i}})),
                    // For an ellipsoid in geodetic latitude, ds^2=M^2*dphi^2 +
                    // N^2*cos(phi)^2*dlambda^2. M,N >= b^2/a, so ellipsoidal
                    // surface distance >= (b^2/a)*unit-sphere angular distance
                    // >= (b^2/a)*unit-sphere chord distance. The distance to a
                    // box containing every candidate vector is smaller still.
                    // Subtract a rounding margin before using it for pruning.
                    radius=ProjEllipsoids[WGS84][b]*ProjEllipsoids[WGS84][b]/ProjEllipsoids[WGS84][a],
                    lower=radius*List.Max({0,Number.Sqrt(List.Sum(List.Transform(gaps,each _*_)))-1e-10})
                in if geodesic then (if sphereBounds=null then null else lower)
                    else if bounds = null then null else Number.Sqrt(dx * dx + dy * dy)*Record.FieldOrDefault(operator,"distanceScale",1),
            search = (current as record, best as list, lowerBound as nullable number) as list =>
                if lowerBound = null then best
                else if List.Count(best) = k and lowerBound > List.Last(best)[dist] then best
                else
                    let
                        candidates = List.Transform(current[shapes], each operator[onCandidate](_, shape)),
                        updatedBest = List.Buffer(operator[combine]({best, candidates})),
                        children = if current[children] = null then {} else current[children],
                        orderedChildren = List.Sort(
                            List.Transform(children, each [node = _, lowerBound = minimumDistance(_)]),
                            each [lowerBound]
                        )
                    in
                        List.Accumulate(orderedChildren, updatedBest, (state, child) =>
                            @search(child[node], state, child[lowerBound])
                        )
        in
            if k = 0 then {}
            else search(node, {}, minimumDistance(node)),
    
	//Queries a quadtree based on a spatial relationship to a supplied shape
	//@param qt - The quadtree to query
	//@param shape - The shape to query against
	//@param op - The query operator defining the spatial relationship
	//@returns List<Record> - A list of query results (each result is typically a record with a shape field)
	QuadTreeQuery = Value.ReplaceType(
        (qt as record, shape as record, op as record) as list =>
            let
                // Geodesic search uses conservative sphere-chord bounds; the
                // candidate metric remains the ellipsoidal surface distance.
                res = if Record.FieldOrDefault(op,"distanceMode",null)="Geodesic" then
                    QuadTreeQueryNearest(if Record.HasFields(qt[root],"geodesicBounds") then qt[root] else QuadTreePrepareGeodesic(qt[root]),shape,op)
                else if Record.HasFields(op, "nearestCount") then
                    QuadTreeQueryNearest(qt[root], shape, op)
                else
                    QuadTreeNodeQuery(qt[root], shape, op),
                listOut = NormalizeList(res)
            in
                listOut,
        type function (qt as TQuadTree, shape as TShape, op as TQuadTreeQueryOperator) as list
    ),

    //Prunes a quadtree node to only include shapes with specified row IDs
    //@param node - The node to prune
    //@param ids as List<Number> - A list of row IDs to keep
    //@returns - The pruned node containing only shapes with matching IDs
    QuadTreeNodePrune = Value.ReplaceType(
        (node as record, ids as list) as record => (
            let
                fromNode = List.Select(node[shapes], each List.Contains(ids, Record.Field(_, "__rowid__"))),
                fromChildren = if node[children] <> null then List.Transform(node[children], (c as record) as record => @QuadTreeNodePrune(c, ids)) else null,
                newNode = Record.TransformFields(node, {{"shapes", each fromNode}, {"children", each fromChildren}})
            in
                QuadTreeUpdateNearestEnvelope(newNode)
        ),
        type function (node as TQuadTreeNode, ids as list) as TQuadTreeNode
    ),

    //**************************************************
    // GIS Layer Implementation (wrapper for quadtree and record data)
    //**************************************************

    //Creates a blank layer with no data
    //@param geometryColumn - The column name that should contain the geometry (default: "shape")
    //@param capacity - The capacity of the quadtree (default: 10)
    //@returns - The created blank layer with an empty table
    LayerCreateBlank = Value.ReplaceType(
        (geometryColumn as nullable text, capacity as nullable number, optional projection as any) as record => (
            let
                geomCol = geometryColumn ?? "shape",
                tbl = #table({"__rowid__", geomCol}, {})
            in
            [
                table = tbl,
                geometryColumn = geomCol,
                queryLayer = QuadTreeCreate(capacity ?? 10, null),
                TProjection = if projection=null then null else ProjResolve(projection)
            ]
        ),
        type function (geometryColumn as nullable text, capacity as nullable number, optional projection as any) as TLayer
    ),

    //Creates a layer from a table with TShape objects
    //@param tbl - The table to create the layer from
    //@param geometryColumn - The column name that contains TShape geometry objects
    //@returns - The created layer with the table and spatial index
    //@remark The geometry column must contain TShape objects, not WKT strings. Use LayerCreateFromTableWithWKT for WKT input.
    LayerCreateFromTable = Value.ReplaceType(
        (tbl as table, geometryColumn as text, optional projection as any) as record => (
            let
                _ = if not Table.HasColumns(tbl, {geometryColumn}) then error "Table does not have the geometry column. Please create the column with the relevant `gisShapeCreateFrom...` functions." else null,
                _2 = if not Table.MatchesAllRows(tbl, each Record.HasFields(_, {"__TShapeIdentifier__"})) then error "Table geometry column does not contain `TShape`s. Please recreate the column with the relevant `gisShapeCreateFrom...` functions." else null,
                tblWithId = EnsureRowIdColumn(tbl),
                qtCapacity = List.Max({10, Table.RowCount(tblWithId) / 10}),
                shapesWithIds = Table.TransformRows(
                    tblWithId,
                    (r as record) as record =>
                        let
                            s = Record.Field(r, geometryColumn),
                            sid = Record.Field(r, "__rowid__"),
                            s2 = if Record.HasFields(s, "__rowid__") then Record.TransformFields(s, {"__rowid__", each sid}) else Record.AddField(s, "__rowid__", sid)
                        in
                            s2
                ),
                inserted = QuadTreeBuild(shapesWithIds, qtCapacity)
            in 
                [
                    table = tblWithId,
                    geometryColumn = geometryColumn,
                    queryLayer = inserted,
                    TProjection = if projection=null then null else ProjResolve(projection)
                ]
        ),
        type function (tbl as table, geometryColumn as text, optional projection as any) as TLayer
    ),

    //Creates a layer from a table with a Well-Known Text (WKT) geometry column
    //@param tbl - The table to create the layer from
    //@param wktColumn - The column name that contains WKT text
    //@returns - The created layer with WKT column transformed to TShape objects
    LayerCreateFromTableWithWKT = Value.ReplaceType(
        (tbl as table, wktColumn as text, optional projection as any) as record => (
            let
                _ = if not Table.HasColumns(tbl, {wktColumn}) then error "Table does not have a column named " & wktColumn & "." else null,
                tblWithShapes = Table.TransformColumns(tbl, {{wktColumn, each ShapeCreateFromWKT(_), type record}}),
                layer = LayerCreateFromTable(tblWithShapes, wktColumn, projection)
            in
                layer
        ),
        type function (tbl as table, wktColumn as text, optional projection as any) as TLayer
    ),

    //Creates a point layer from a table with numeric X and Y coordinate columns
    //@param tbl - The table to create the layer from
    //@param xColumn - The column name that contains X coordinates
    //@param yColumn - The column name that contains Y coordinates
    //@returns - The created layer with the original columns and a new shape column
    LayerCreateFromTableWithXY = Value.ReplaceType(
        (tbl as table, xColumn as text, yColumn as text, optional projection as any) as record => (
            let
                validatedTable =
                    if not Table.HasColumns(tbl, {xColumn, yColumn}) then
                        error "Table must contain the coordinate columns " & xColumn & " and " & yColumn & "."
                    else if Table.HasColumns(tbl, {"shape"}) then
                        error "Table already has a column named shape. Rename it before creating a layer from XY coordinates."
                    else
                        tbl,
                tblWithShapes = Table.AddColumn(
                    validatedTable,
                    "shape",
                    each ShapeCreatePointFromLatLng(Record.Field(_, yColumn), Record.Field(_, xColumn)),
                    type record
                )
            in
                LayerCreateFromTable(tblWithShapes, "shape", projection)
        ),
        type function (tbl as table, xColumn as text, yColumn as text, optional projection as any) as TLayer
    ),

    //Inserts rows into a layer and updates the spatial index
    //@param layer - The layer to insert the rows into
    //@param rows as List<Record> - The list of records (rows) to insert
    //@returns - The updated layer with the inserted rows
    //@remark Rows should have the geometry column populated with TShape objects
	LayerInsertRows = Value.ReplaceType(
		(layer as record, rows as list) as record => (
			let
				baseTable = EnsureRowIdColumn(layer[table]),
				nextId0 = if Table.RowCount(baseTable) = 0 then 0 else 1 + List.Max(Table.Column(baseTable, "__rowid__")),
				// 1) Ensure __rowid__ present on all rows, assigning sequentially from nextId0 when missing
				assignIds = (state as record, r as record) as record => (
					let
						rid = state[nextId],
						rWithId = if Record.HasFields(r, "__rowid__") then Record.TransformFields(r, {"__rowid__", each rid}) else Record.AddField(r, "__rowid__", rid),
						nextId1 = state[nextId] + 1
					in
						[rowsOut = state[rowsOut] & {rWithId}, nextId = nextId1]
				),
				assigned = List.Accumulate(rows ?? {}, [rowsOut = {}, nextId = nextId0], assignIds),
				rowsWithIds = assigned[rowsOut],
				// 2) Ensure shapes have corresponding __rowid__
				shapesWithIds = List.Transform(
					rowsWithIds,
					(r as record) as record => (
						let
							s0 = Record.Field(r, layer[geometryColumn]),
							rid = Record.Field(r, "__rowid__"),
							s1 = if Record.HasFields(s0, "__rowid__") then Record.TransformFields(s0, {"__rowid__", each rid}) else Record.AddField(s0, "__rowid__", rid)
						in
							s1
					)
				),
				// 3) Recursively add shapes to quadtree (one-by-one, preserving insert semantics)
				qt1 = List.Accumulate(shapesWithIds, layer[queryLayer], QuadTreeInsert),
				// 4) Insert all prepared rows into the table in a single batch
				newTable = if List.Count(rowsWithIds) = 0 then baseTable else Table.InsertRows(baseTable, 0, rowsWithIds)
			in
				[
					table = newTable,
					geometryColumn = layer[geometryColumn],
					queryLayer = qt1,
					TProjection = layer[TProjection]
				]
		),
		type function (layer as TLayer, rows as list) as TLayer
	),

    //Queries a layer based on a row-wise relational operator (attribute-based filtering)
    //@param layer - The layer to query
    //@param operator => Logical - A function that takes a record and returns true/false
    //@returns - A new layer containing only the rows that match the relational query
    //@remark This is equivalent to Table.SelectRows but also updates the spatial index
    LayerQueryRelational = Value.ReplaceType(
        (layer as record, operator as function) as record => (
            let
                selected = Table.SelectRows(layer[table], each operator(_)),
                selectedIDs = Table.Column(selected, "__rowid__"),
                filteredQuadTree = [
                    root = QuadTreeNodePrune(layer[queryLayer][root], selectedIDs),
                    capacity = layer[queryLayer][capacity]
                ]
            in
                [
                    table = selected,
                    geometryColumn = layer[geometryColumn],
                    queryLayer = filteredQuadTree,
                    TProjection = layer[TProjection]
                ]
        ),
        type function (layer as TLayer, operator as TRowQueryOperator) as TLayer
    ),



    //Join two layers together based on a spatial relationship
    //@param layer1 - The first layer to join
    //@param layer2 - The second layer to join
    //@param gisOperator - The spatial relationship to use for the join. If not provided, the default is Intersect.
    //                     Remark: Read like "(shapes of layer1) within (shapes of layer2)"
    //@param joinType - The type of join to perform. If not provided, the default is "Inner".
    //                  Remark: Supported types are "Inner", "Left Outer", "Right Outer", "Full Outer"
    //@return The joined layer, the table of which has 4 columns: __rowid__, layer1, layer2, shape. Where layer1 and layer2 are the row objects from the original layers, and shape is the shape of the row from the original layer.
    LayerJoinSpatialCore = Value.ReplaceType(
        (layer1 as record, layer2 as record, gisOperator as nullable record, joinType as nullable text) as record => (
            let
                actualJoinType = joinType ?? "Inner",
                actualGisOperator = gisOperator ?? [onCandidate = QuadTreeOperatorEnvelopeIntersects],
                table1 = layer1[table],
                table2 = layer2[table],
                geomCol1 = layer1[geometryColumn],
                geomCol2 = layer2[geometryColumn],
                
                // For each row in layer1, find matching rows in layer2
                layer1Matches = Table.TransformRows(
                    table1,
                    (r1 as record) as record => (
                        let
                            shape1 = Record.Field(r1, geomCol1),
                            matchingShapes = QuadTreeQuery(layer2[queryLayer], shape1, actualGisOperator),
                            matchingIds =
                                List.Transform(
                                    matchingShapes,
                                    each _[shape][__rowid__]
                                ),
                            matchingRows = Table.SelectRows(table2, (r2 as record) as logical => List.Contains(matchingIds, Record.Field(r2, "__rowid__")))
                        in
                            [row1 = r1, matchingRows = matchingRows, matchingShapes = matchingShapes, shape1 = shape1]
                    )
                ),
                
                // Build result based on join type
                resultRows = (
                    if actualJoinType = "Inner" then
                        // Inner join: only rows with matches
                        List.Combine(
                            List.Transform(
                                layer1Matches,
                                (match as record) as list =>
                                    let
                                        r1 = match[row1],
                                        matches = Table.ToRecords(match[matchingRows]),
                                        shapes = match[matchingShapes]
                                    in
                                        if List.Count(matches) > 0 then
                                            List.Transform(
                                                matches,
                                                (r2 as record) as record =>
                                                    let
                                                        r2id = r2[__rowid__],
                                                        shapeData = if List.Count(shapes) > 0 then List.First(List.Select(shapes, each _[shape][__rowid__] = r2id)) else null,
                                                        shapeExtras = if shapeData <> null then Record.RemoveFields(shapeData, {"shape"}) else [],
                                                        actualShape = if shapeData <> null then shapeData[shape] else null
                                                    in
                                                        Record.Combine({
                                                            [ 
                                                                layer1 = r1,
                                                                layer2 = r2,
                                                                shape  = actualShape
                                                            ],
                                                            shapeExtras
                                                        })
                                            )
                                        else
                                            {}
                            )
                        )

                    else if actualJoinType = "Left Outer" then
                        // Left Outer: all rows from layer1
                        List.Combine(
                            List.Transform(
                                layer1Matches,
                                (match as record) as list =>
                                    let
                                        r1 = match[row1],
                                        matches = Table.ToRecords(match[matchingRows]),
                                        shapes = match[matchingShapes]
                                    in
                                        if List.Count(matches) > 0 then
                                            List.Transform(
                                                matches,
                                                (r2 as record) as record =>
                                                    let
                                                        r2id = r2[__rowid__],
                                                        shapeData = if List.Count(shapes) > 0 then List.First(List.Select(shapes, each _[shape][__rowid__] = r2id)) else null,
                                                        shapeExtras = if shapeData <> null then Record.RemoveFields(shapeData, {"shape"}) else [],
                                                        actualShape = if shapeData <> null then shapeData[shape] else null
                                                    in
                                                        Record.Combine({
                                                            [ 
                                                                layer1 = r1,
                                                                layer2 = r2,
                                                                shape  = actualShape
                                                            ],
                                                            shapeExtras
                                                        })
                                            )
                                        else
                                            {
                                                [ 
                                                    layer1 = r1,
                                                    layer2 = null,
                                                    shape  = match[shape1]
                                                ]
                                            }
                            )
                        )

                    else if actualJoinType = "Right Outer" then
                        // Right Outer: all rows from layer2
                        let
                            matchedLayer2Ids =
                                List.Distinct(
                                    List.Combine(
                                        List.Transform(
                                            layer1Matches,
                                            (match as record) as list =>
                                                Table.Column(match[matchingRows], "__rowid__")
                                        )
                                    )
                                ),
                            // Rows with matches
                            matchedRows =
                                List.Combine(
                                    List.Transform(
                                        layer1Matches,
                                        (match as record) as list =>
                                            let
                                                r1 = match[row1],
                                                matches = Table.ToRecords(match[matchingRows]),
                                                shapes = match[matchingShapes]
                                            in
                                                List.Transform(
                                                    matches,
                                                    (r2 as record) as record =>
                                                        let
                                                            r2id = r2[__rowid__],
                                                            shapeData = if List.Count(shapes) > 0 then List.First(List.Select(shapes, each _[shape][__rowid__] = r2id)) else null,
                                                            shapeExtras = if shapeData <> null then Record.RemoveFields(shapeData, {"shape"}) else [],
                                                            extras = Record.RemoveFields(r2, {geomCol2, "__rowid__"}),
                                                            actualShape = if shapeData <> null then shapeData[shape] else null
                                                        in
                                                            Record.Combine({
                                                                [ 
                                                                    layer1 = r1,
                                                                    layer2 = r2,
                                                                    shape  = actualShape
                                                                ],
                                                                extras,
                                                                shapeExtras
                                                            })
                                                )
                                    )
                                ),
                            // Unmatched rows from layer2
                            unmatchedRows =
                                Table.TransformRows(
                                    Table.SelectRows(
                                        table2,
                                        (r2 as record) as logical =>
                                            not List.Contains(matchedLayer2Ids, r2[__rowid__])
                                    ),
                                    (r2 as record) as record => [
                                        layer1 = null,
                                        layer2 = r2,
                                        shape  = r2[geomCol2]
                                    ]
                                )
                        in
                            matchedRows & unmatchedRows

                    else if actualJoinType = "Full Outer" then
                        let
                            matchedLayer2Ids =
                                List.Distinct(
                                    List.Combine(
                                        List.Transform(
                                            layer1Matches,
                                            (match as record) as list =>
                                                Table.Column(match[matchingRows], "__rowid__")
                                        )
                                    )
                                ),
                            layer1Results =
                                List.Combine(
                                    List.Transform(
                                        layer1Matches,
                                        (match as record) as list =>
                                            let
                                                r1 = match[row1],
                                                matches = Table.ToRecords(match[matchingRows]),
                                                shapes = match[matchingShapes]
                                            in
                                                if List.Count(matches) > 0 then
                                                    List.Transform(
                                                        matches,
                                                        (r2 as record) as record =>
                                                            let
                                                                r2id = r2[__rowid__],
                                                                shapeData = if List.Count(shapes) > 0 then List.First(List.Select(shapes, each _[shape][__rowid__] = r2id)) else null,
                                                                shapeExtras = Record.RemoveFields(shapeData, {"shape"}),
                                                                extras = Record.RemoveFields(r2, {geomCol2, "__rowid__"}),
                                                                actualShape = shapeData[shape]
                                                            in
                                                                Record.Combine({
                                                                    [ layer1 = r1,
                                                                    layer2 = r2,
                                                                    shape  = actualShape
                                                                    ],
                                                                    extras,
                                                                    shapeExtras
                                                                })
                                                    )
                                                else
                                                    {
                                                        [ layer1 = r1,
                                                        layer2 = null,
                                                        shape  = match[shape1]
                                                        ]
                                                    }
                                    )
                                ),
                            unmatchedLayer2 =
                                Table.TransformRows(
                                    Table.SelectRows(
                                        table2,
                                        (r2 as record) as logical =>
                                            not List.Contains(matchedLayer2Ids, r2[__rowid__])
                                    ),
                                    (r2 as record) as record => [
                                        layer1 = null,
                                        layer2 = r2,
                                        shape  = r2[geomCol2]
                                    ]
                                )
                        in
                            layer1Results & unmatchedLayer2

                    else
                        error "Invalid join type"
                ),

                // Normalize records to ensure consistent schema
                normalizedRows = NormalizeRecords(resultRows, actualGisOperator[additionalColumns]),
                
                // Create result table with __rowid__ column
                resultColumns = List.Distinct(List.Combine({{"layer1","layer2","shape"},actualGisOperator[additionalColumns] ?? {},List.Combine(List.Transform(normalizedRows,Record.FieldNames))})),
                resultTable0 = Table.FromRecords(normalizedRows, resultColumns, MissingField.UseNull),
                resultTable = Table.AddIndexColumn(resultTable0, "__rowid__", 0, 1, Int64.Type),
                baseColumns = {"__rowid__", "layer1", "layer2", "shape"},
                allColumns = Table.ColumnNames(resultTable),
                extraColumns = List.RemoveItems(allColumns, baseColumns),
                columnOrder = baseColumns & extraColumns,
                resultTableReordered = Table.ReorderColumns(resultTable, columnOrder),
                
                // Build quadtree for the result
                qtCapacity = if List.Count(resultRows) > 100 then List.Count(resultRows) / 10 else 10,
                qt = QuadTreeCreate(qtCapacity, null),
                shapesWithIds = Table.TransformRows(
                    resultTableReordered,
                    (r as record) as record =>
                        let
                            s = r[shape],
                            sid = r[__rowid__],
                            s2 = 
                                if Record.HasFields(s, "__rowid__") then 
                                    Record.TransformFields(s, {"__rowid__", each sid}) 
                                else 
                                    if sid <> null then
                                        Record.AddField(s, "__rowid__", sid)
                                    else
                                        s
                        in
                            s2
                ),
                inserted = List.Accumulate(shapesWithIds, qt, QuadTreeInsert)
            in
                [
                    table = resultTableReordered,
                    geometryColumn = "shape",
                    queryLayer = inserted,
                    TProjection = layer1[TProjection]
                ]
        ),
        type function (layer1 as TLayer, layer2 as TLayer, gisOperator as nullable TQuadTreeQueryOperator, joinType as nullable text) as TLayer
    ),

    LayerCreateFromTableWithGeoJSON = (tbl as table, geometryColumn as text, optional projection as any) as record =>
        if not Table.HasColumns(tbl,{geometryColumn}) then ProjError("GeoJSON geometry column is missing.")
        else LayerCreateFromTable(Table.TransformColumns(tbl,{{geometryColumn,ShapeCreateFromGeoJSON,type record}}),geometryColumn,projection ?? ProjEPSG[#"EPSG:4326"]),

    LayerReproject = (layer as record, target as any, optional options as nullable record) as record =>
        let
            source=layer[TProjection], crs=ProjResolve(target), opts=options ?? [],
            projected=Table.TransformColumns(layer[table],{{layer[geometryColumn],each ShapeReproject(_,source,crs,opts),type record}}),
            rebuilt=LayerCreateFromTable(projected,layer[geometryColumn],crs)
        in if source=null then ProjError("Reprojection requires the layer's source CRS to be declared.")
            else if not Record.HasFields(source,"IntoWGS84") and not Record.HasFields(crs,"IntoWGS84") and source=crs then layer else rebuilt,

    // Prepare one common CRS and metric before querying. The original source
    // records remain in layer1/layer2; the result shape/index use the analysis CRS.
    LayerJoinSpatial = (layer1 as record, layer2 as record, gisOperator as nullable record, joinType as nullable text, optional options as nullable record) as record =>
        let
            opts=options ?? [], c1=layer1[TProjection],c2=layer2[TProjection],
            op0=gisOperator ?? QuadTreeOperatorIntersects,
            nearest=Record.HasFields(op0,"nearestCount"),
            requested=Record.FieldOrDefault(opts,"mode",Record.FieldOrDefault(op0,"distanceMode",null)),
            mode=if requested<>null then requested else if explicitCRS<>null then "Planar"
                else if nearest and c1<>null and c2<>null and c1[IsGeographic] and c2[IsGeographic] then "Geodesic" else "Planar",
            explicitCRS=Record.FieldOrDefault(opts,"analysisCRS",null),
            known=c1<>null and c2<>null,
            target=if not known then null else if mode="Geodesic" then ProjEPSG[#"EPSG:4326"]
                else if explicitCRS<>null then ProjResolve(explicitCRS)
                else if c1[IsGeographic] and not c2[IsGeographic] then c2 else c1,
            a=if known then LayerReproject(layer1,target,opts) else layer1,
            b0=if known then LayerReproject(layer2,target,opts) else layer2,
            b=if mode="Geodesic" then Record.TransformFields(b0,{{"queryLayer",each Record.TransformFields(_,{{"root",QuadTreePrepareGeodesic}})}}) else b0,
            // Scale the candidate metric AND tree lower bounds together.
            // Geographic nearest uses sphere bounds and an ellipsoidal metric.
            op=if mode="Geodesic" then
                    if Record.FieldOrDefault(op0,"distanceMode",null)="Geodesic" then op0
                    else QuadTreeOperatorNearestGeodesicN(op0[nearestCount])
                else if nearest and target<>null then Record.Combine({op0,[
                    distanceScale=target[UnitToMeter],
                    onCandidate=(candidate,query)=>let result=op0[onCandidate](candidate,query) in
                        Record.TransformFields(result,{{"dist",each _*target[UnitToMeter]}})
                ]}) else op0,
            joined=LayerJoinSpatialCore(a,b,op,joinType),
            rows1=Record.FromList(Table.ToRecords(layer1[table]),List.Transform(Table.Column(layer1[table],"__rowid__"),each Text.From(_,"en-US"))),
            rows2=Record.FromList(Table.ToRecords(layer2[table]),List.Transform(Table.Column(layer2[table],"__rowid__"),each Text.From(_,"en-US"))),
            restored=Table.TransformColumns(joined[table],{
                {"layer1",each if _=null then null else Record.Field(rows1,Text.From(_[__rowid__],"en-US"))},
                {"layer2",each if _=null then null else Record.Field(rows2,Text.From(_[__rowid__],"en-US"))}
            })
        in if (c1=null)<>(c2=null) then ProjError("Both layers must declare a CRS when either layer has one.")
            else if not known and (explicitCRS<>null or requested<>null) then ProjError("An analysis CRS/distance mode requires both source CRSs to be declared.")
            else if not List.Contains({"Planar","Geodesic"},mode) then ProjError("Distance mode must be Planar or Geodesic.")
            else if mode="Geodesic" and not nearest then ProjError("Geodesic mode currently supports nearest-neighbour point queries only.")
            else if mode="Geodesic" and explicitCRS<>null then ProjError("Geodesic mode uses WGS84; analysisCRS is only used in Planar mode.")
            else if nearest and target<>null and mode="Planar" and target[IsGeographic] then ProjError("Planar nearest-neighbour distances require a projected analysis CRS.")
            else Record.TransformFields(joined,{{"table",each restored}}),

    LayerQuerySpatial = (layer as record, shape as record, gisOperator as record, optional projection as any, optional options as nullable record) as record =>
        let
            queryLayer=LayerCreateFromTable(#table({"shape"},{{shape}}),"shape",projection ?? layer[TProjection]),
            joined=LayerJoinSpatial(queryLayer,layer,gisOperator,"Inner",options),
            extras=gisOperator[additionalColumns] ?? {},
            rows=Table.TransformRows(joined[table],each Record.Combine({_[layer2],Record.SelectFields(_,extras)})),
            cols=List.Union({Table.ColumnNames(layer[table]),extras}),
            table=Table.FromRecords(rows,cols,MissingField.UseNull)
        in LayerCreateFromTable(table,layer[geometryColumn],layer[TProjection])
in
    [
        proj = [
            fromEPSG = ProjEPSG,
            fromJSON = ProjFromJSON,
            fromProj4 = ProjFromProj4,
            transform = ProjTransform,
            distance = ProjDistance,
            withDatumTransform = ProjWithDatumTransform,
            supportedMethods = Record.FieldNames(ProjMethods)
        ],
        gisShapeCreateFromWKT = ShapeCreateFromWKT,
        gisShapeCreateFromGeoJSON = ShapeCreateFromGeoJSON,
        gisShapeReproject = ShapeReproject,
        gisLayerCreateBlank = LayerCreateBlank,
        gisLayerCreateFromTable = LayerCreateFromTable,
        gisLayerCreateFromTableWithWKT = LayerCreateFromTableWithWKT,
        gisLayerCreateFromTableWithXY = LayerCreateFromTableWithXY,
        gisLayerCreateFromTableWithGeoJSON = LayerCreateFromTableWithGeoJSON,
        gisLayerReproject = LayerReproject,
        gisLayerQuerySpatial = LayerQuerySpatial,
        gisLayerQueryRelational = LayerQueryRelational,
        gisLayerQueryOperators = [
            gisEnvelopeIntersects = QuadTreeOperatorEnvelopeIntersects,
            gisEnvelopeContains = QuadTreeOperatorEnvelopeContains,
            gisEnvelopeWithin = QuadTreeOperatorEnvelopeWithin,
            gisIntersects = QuadTreeOperatorIntersects,
            gisContains = QuadTreeOperatorContains,
            gisWithin = QuadTreeOperatorWithin,
            gisQueryOperatorType = TQuadTreeQueryOperator, //Provided for custom operators
            gisNearestN = QuadTreeOperatorNearestN,
            gisNearest = QuadTreeOperatorNearest,
            gisNearestGeodesicN = QuadTreeOperatorNearestGeodesicN,
            gisNearestGeodesic = QuadTreeOperatorNearestGeodesicN(1)
        ],
		gisLayerInsertRows = LayerInsertRows,
		gisLayerJoinSpatial = LayerJoinSpatial
    ]



