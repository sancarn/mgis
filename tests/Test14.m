let
    shape=mgis[gisShapeCreateFromWKT],voronoi=mgis[gisShapeCreateVoronoi],
    bounds=[MinX=-5,MinY=-5,MaxX=15,MaxY=15],
    cells=(wkts,optional resolution)=>voronoi(List.Transform(wkts,shape),bounds,[resolution=resolution ?? 10]),
    area=(g)=>if g[Kind]="MULTIPOLYGON" then List.Sum(List.Transform(g[Components],each @area(_))) else
        List.Sum(List.Transform(g[Rings],(r)=>let p=r[Points] in List.Sum(List.Transform(List.Skip(List.Positions(p)),
            (i)=>p{i-1}[X]*p{i}[Y]-p{i}[X]*p{i-1}[Y]))/2)),
    total=(cs)=>List.Sum(List.Transform(List.RemoveNulls(cs),each area([Geometry]))),
    inside=(s,x,y)=>if s=null then false else let g=s[Geometry],p=[Kind="POINT",X=x,Y=y],
        polygons=if g[Kind]="POLYGON" then {g} else g[Components],
        inRing=(ring)=>let pts=ring[Points],crossings=List.Sum(List.Transform(List.Skip(List.Positions(pts)),(i)=>let
            a=pts{i-1},b=pts{i} in if ((a[Y]>y)<>(b[Y]>y)) and x<(b[X]-a[X])*(y-a[Y])/(b[Y]-a[Y])+a[X] then 1 else 0))
            in Number.Mod(crossings,2)=1
        in List.MatchesAny(polygons,each inRing([Rings]{0}) and not List.MatchesAny(List.Skip([Rings]),inRing)),
    points=cells({"POINT(0 0)","POINT(10 0)"}),
    parallel=cells({"LINESTRING(0 -10,0 20)","LINESTRING(10 -10,10 20)"}),
    curve=cells({"POINT(5 5)","LINESTRING(-10 0,20 0)"}),
    curve3=cells({"POINT(5 5)","LINESTRING(-10 0,20 0)"},3),
    squares=cells({"POLYGON((0 0,2 0,2 10,0 10,0 0))","POLYGON((8 0,10 0,10 10,8 10,8 0))"}),
    duplicate=cells({"POINT(0 0)","POINT(0 0)"}),
    duplicateLine=cells({"LINESTRING(0 0,10 0)","LINESTRING(0 0,10 0)"}),
    crossing=cells({"LINESTRING(0 0,10 10)","LINESTRING(0 10,10 0)"}),
    overlap=cells({"POLYGON((0 0,8 0,8 10,0 10,0 0))","POLYGON((4 0,12 0,12 10,4 10,4 0))"}),
    nested=cells({"POLYGON((3 3,7 3,7 7,3 7,3 3))","POLYGON((0 0,10 0,10 10,0 10,0 0))"}),
    holed=cells({"POLYGON((0 0,10 0,10 10,0 10,0 0),(2 2,8 2,8 8,2 8,2 2))","POINT(5 5)"}),
    multi=cells({"MULTIPOINT((0 0),(10 0))","POINT(5 0)"}),
    collection=cells({"GEOMETRYCOLLECTION(POINT(0 0),LINESTRING(10 0,10 10))","POINT(5 5)"}),
    triple=cells({"POINT(0 0)","POINT(10 0)","POINT(5 10)"}),
    collinear=cells({"LINESTRING(0 0,8 0)","LINESTRING(4 0,12 0)"}),
    reversedLine=cells({"LINESTRING(-1.234 0.321,10.454 9.343)","LINESTRING(10.454 9.343,-1.234 0.321)"}),
    sharedEndpoint=cells({"LINESTRING(0 0,10 0)","LINESTRING(0 0,0 10)"}),
    bent=cells({"LINESTRING(0 0,10 0,10 10)","POINT(3 6)"},20),
    multiLine=cells({"MULTILINESTRING((0 -10,0 20),(10 -10,10 20))","POINT(5 5)"}),
    multiPolygon=cells({"MULTIPOLYGON(((0 0,2 0,2 2,0 2,0 0)),((8 8,10 8,10 10,8 10,8 8)))","POINT(5 5)"}),
    // Independent point-to-segment formula, separate from the implementation's
    // primitive kernels, checks ownership against the entire bent polyline.
    bentDistance=(x,y)=>List.Min({Number.Sqrt(Number.Power(x-List.Max({0,List.Min({10,x})}),2)+y*y),
        Number.Sqrt(Number.Power(x-10,2)+Number.Power(y-List.Max({0,List.Min({10,y})}),2))}),
    samples=List.Combine(List.Transform({-3,-1,1,3,5,7,9,11,13},(x)=>List.Transform({-3,-1,1,3,5,7,9,11,13},(y)=>let
        a=bentDistance(x,y),b=Number.Sqrt(Number.Power(x-3,2)+Number.Power(y-6,2)),
        owners=List.Select({0,1},each inside(bent{_},x,y))
        in if Number.Abs(a-b)<0.2 then true else List.Count(owners)=1 and owners{0}=(if a<b then 0 else 1)))),
    curveVertices=List.Distinct(List.Select(curve{0}[Geometry][Rings]{0}[Points],each [Y]<14.9)),
    web=mgis[proj][fromEPSG][#"EPSG:3857"],wgs=mgis[proj][fromEPSG][#"EPSG:4326"],
    layer=mgis[gisLayerCreateFromTableWithWKT](#table({"Name","geom","__rowid__"},{{"A","POINT(0 0)",42},{"B","POINT(10 0)",99}}),"geom",web),
    layerCells=mgis[gisLayerCreateVoronoi](layer,bounds,[resolution=10]),
    geographic=mgis[gisLayerCreateFromTableWithWKT](#table({"Name","geom"},{{"A","POINT(0 0)"},{"B","POINT(0.00008983152841195215 0)"}}),"geom",wgs),
    projectedCells=mgis[gisLayerCreateVoronoi](geographic,bounds,[analysisCRS=web]),
    checks=[
        PointBisector=points{0}[Envelope][MaxX]=5 and points{1}[Envelope][MinX]=5 and Number.Abs(total(points)-400)<1e-6,
        ParallelLines=parallel{0}[Envelope][MaxX]=5 and parallel{1}[Envelope][MinX]=5 and Number.Abs(total(parallel)-400)<1e-6,
        Parabola=inside(curve{0},5,4) and not inside(curve{0},5,1) and inside(curve{1},5,1) and Number.Abs(total(curve)-400)<1e-6,
        CurveResolution=List.Count(curve{0}[Geometry][Rings]{0}[Points])-List.Count(curve3{0}[Geometry][Rings]{0}[Points])=7,
        CurveCoordinates=List.Count(curveVertices)=10 and List.AllTrue(List.Transform(curveVertices,each Number.Abs([Y]-(2.5+Number.Power([X]-5,2)/10))<1e-6)),
        StraightEdgesUnchanged=List.Count(points{0}[Geometry][Rings]{0}[Points])=5,
        PolygonSites=inside(squares{0},1,5) and inside(squares{1},9,5) and Number.Abs(total(squares)-400)<1e-6,
        DuplicateTie=duplicate{1}=null and Number.Abs(area(duplicate{0}[Geometry])-400)<1e-6,
        DuplicateLineTie=duplicateLine{1}=null and Number.Abs(total(duplicateLine)-400)<1e-6,
        CrossingLines=inside(crossing{0},1,1) and inside(crossing{1},1,9) and Number.Abs(total(crossing)-400)<1e-6,
        OverlapTie=inside(overlap{0},6,5) and not inside(overlap{1},6,5) and inside(overlap{1},10,5) and Number.Abs(total(overlap)-400)<1e-6,
        NestedPolygons=inside(nested{0},5,5) and not inside(nested{1},5,5) and inside(nested{1},1,1) and Number.Abs(total(nested)-400)<1e-6,
        PolygonHole=inside(holed{1},5,5) and not inside(holed{0},5,5) and inside(holed{0},1,5) and Number.Abs(total(holed)-400)<1e-6,
        MultipartSite=multi{0}[Kind]="MULTIPOLYGON" and inside(multi{0},0,0) and inside(multi{0},10,0) and inside(multi{1},5,0) and Number.Abs(total(multi)-400)<1e-6,
        GeometryCollection=inside(collection{0},0,0) and inside(collection{0},10,5) and inside(collection{1},5,5) and Number.Abs(total(collection)-400)<1e-6,
        TripleVertex=Number.Abs(total(triple)-400)<1e-6 and inside(triple{2},5,10),
        CollinearOverlap=inside(collinear{0},6,5) and inside(collinear{1},10,5) and Number.Abs(total(collinear)-400)<1e-6,
        ReversedDuplicateTie=reversedLine{1}=null and Number.Abs(total(reversedLine)-400)<1e-6,
        SharedEndpoint=inside(sharedEndpoint{0},-1,-1) and inside(sharedEndpoint{1},-1,5) and Number.Abs(total(sharedEndpoint)-400)<1e-6,
        BentLineOwnership=List.AllTrue(samples) and Number.Abs(total(bent)-400)<1e-6,
        MultiLine=inside(multiLine{0},0,5) and inside(multiLine{0},10,5) and inside(multiLine{1},5,5) and Number.Abs(total(multiLine)-400)<1e-6,
        MultiPolygon=inside(multiPolygon{0},1,1) and inside(multiPolygon{0},9,9) and inside(multiPolygon{1},5,5) and Number.Abs(total(multiPolygon)-400)<1e-6,
        LayerMetadata=layerCells[geometryColumn]="geom" and layerCells[TProjection]=web and layerCells[table][Name]={"A","B"}
            and layerCells[table][__rowid__]={42,99} and layerCells[table]{0}[geom][__rowid__]=42,
        IndexRebuilt=Table.RowCount(mgis[gisLayerQuerySpatial](layerCells,shape("POINT(1 1)"),mgis[gisLayerQueryOperators][gisContains])[table])=1,
        AnalysisCRS=projectedCells[TProjection]=web and Number.Abs(projectedCells[table]{0}[geom][Envelope][MaxX]-5)<1e-6,
        GeographicRejected=(try mgis[gisLayerCreateVoronoi](geographic,bounds))[HasError],
        GeographicAnalysisRejected=(try mgis[gisLayerCreateVoronoi](layer,bounds,[analysisCRS=wgs]))[HasError],
        UnknownCRSRejected=(try mgis[gisLayerCreateVoronoi](mgis[gisLayerCreateBlank]("shape",10),bounds,[analysisCRS=web]))[HasError],
        EmptyLayer=Table.RowCount(mgis[gisLayerCreateVoronoi](mgis[gisLayerCreateBlank]("geom",10,web),bounds)[table])=0,
        EmptyLayerValidation=(try mgis[gisLayerCreateVoronoi](mgis[gisLayerCreateBlank]("geom",10,web),bounds,[resolution=2]))[HasError]
            and (try mgis[gisLayerCreateVoronoi](mgis[gisLayerCreateBlank]("geom",10,web),[MinX=0,MinY=0,MaxX=0,MaxY=1]))[HasError],
        LayerDropsEmptyCells=Table.RowCount(mgis[gisLayerCreateVoronoi](mgis[gisLayerCreateFromTableWithWKT](
            #table({"Name","geom"},{{"First","POINT(0 0)"},{"Duplicate","POINT(0 0)"}}),"geom"),bounds)[table])=1,
        ResolutionRange=List.AllTrue(List.Transform({3,10,100},(n)=>List.Count(cells({"POINT(5 5)","LINESTRING(-10 0,20 0)"},n){0}[Geometry][Rings]{0}[Points])=n+3)),
        SingleSite=Number.Abs(total(cells({"LINESTRING(0 0,10 10)"}))-400)<1e-6,
        Empty=voronoi({},bounds)={},
        BadBounds=(try voronoi({},[MinX=0,MinY=0,MaxX=0,MaxY=1]))[HasError],
        NonFiniteBounds=(try voronoi({},[MinX=0,MinY=0,MaxX=#infinity,MaxY=1]))[HasError],
        TinyBounds=(try voronoi({shape("POINT(1000000000 0)")},[MinX=0,MinY=0,MaxX=1,MaxY=1]))[HasError],
        BadResolution=List.AllTrue(List.Transform({2,101,3.5,#nan,#infinity},each (try voronoi({},bounds,[resolution=_]))[HasError]))
    ],
    evaluated=Record.FromList(List.Transform(Record.FieldNames(checks),(name)=>let result=try Record.Field(checks,name)
        in if result[HasError] then result[Error][Message] else result[Value]),Record.FieldNames(checks))
in
    if List.AllTrue(List.Transform(Record.FieldValues(evaluated),each _=true)) then evaluated
    else error Error.Record("TestFailure",Text.FromBinary(Json.FromValue(evaluated)),null)
