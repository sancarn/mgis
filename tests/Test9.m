let
    proj=mgis[proj],wgs=proj[fromEPSG][#"EPSG:4326"],ops=mgis[gisLayerQueryOperators],
    xy=mgis[gisLayerCreateFromTableWithXY],join=mgis[gisLayerJoinSpatial],
    data=List.Transform({0..95},(i)=>{i,Number.Mod(i*73,359)-179,Number.Mod(i*31,151)-75}),
    targets=xy(#table({"ID","X","Y"},data),"X","Y",wgs),
    queries={{-179.9,10},{179.9,10},{0,89.9},{0,-89.9},{-30,30},{100,-40}},
    agreements=List.Combine(List.Transform(queries,(point)=>List.Transform({1,3,12},(k)=>let
        source=xy(#table({"X","Y"},{{point{0},point{1}}}),"X","Y",wgs),
        expected=List.FirstN(List.Sort(List.Transform(data,(row)=>proj[distance](point,{row{1},row{2}},wgs))),k),
        actual=List.Sort(Table.Column(join(source,targets,ops[gisNearestN](k),"Inner")[table],"dist"))
        in List.Count(actual)=List.Count(expected) and List.AllTrue(List.Transform(List.Positions(expected),(i)=>Number.Abs(actual{i}-expected{i})<0.001))
    ))),
    // Controlled branches prove that geodesic search does NOT evaluate every
    // candidate, including when the nearest branch is across the date line.
    fixtureLayer=xy(#table({"ID","X","Y"},{{"Near",-179.99,10},{"Far",0,0},{"Far2",120,-70}}),"X","Y",wgs),
    rows=Table.ToRecords(fixtureLayer[table]),
    fixtureShapes=List.Transform(rows,(row)=>Record.TransformFields(row[shape],{{"__rowid__",each row[__rowid__]}})),
    leaf=(shape)=>[envelope=shape[Envelope],shapes={shape},children=null,capacity=10],
    fixture=Record.TransformFields(fixtureLayer,{{"queryLayer",each [capacity=10,root=[
        envelope=[MinX=-180,MinY=-90,MaxX=180,MaxY=90],capacity=10,shapes={},children=List.Transform(fixtureShapes,leaf)
    ]]}}),
    guarded=Record.Combine({ops[gisNearestGeodesic],[onCandidate=(candidate,query)=>
        if candidate[__rowid__]<>0 then error "Distant geodesic candidate was evaluated instead of pruned."
        else ops[gisNearestGeodesic][onCandidate](candidate,query)]}),
    query=xy(#table({"X","Y"},{{179.99,10}}),"X","Y",wgs),
    pruned=join(query,fixture,guarded,"Inner"),
    utm=proj[fromProj4]("+proj=utm +zone=31 +datum=WGS84"),
    geographicPlanar=join(xy(#table({"X","Y"},{{3,50}}),"X","Y",wgs),xy(#table({"X","Y"},{{3.001,50}}),"X","Y",wgs),ops[gisNearest],"Inner",[analysisCRS=utm]),
    duplicates=xy(#table({"ID","X","Y"},List.Transform({0..11},each {_,1,2})),"X","Y",wgs),
    duplicatedNearest=join(xy(#table({"X","Y"},{{1,2}}),"X","Y",wgs),duplicates,ops[gisNearestN](12),"Inner"),
    checks=[
        ExhaustiveAgreement=List.AllTrue(agreements),
        DistantGeodesicBranchesPruned=pruned[table]{0}[layer2][ID]="Near" and pruned[table]{0}[dist]<2500,
        GeographicInputsPlanarAnalysis=geographicPlanar[TProjection]=utm and geographicPlanar[table]{0}[dist]>70 and geographicPlanar[table]{0}[dist]<73,
        DuplicatePoints=Table.RowCount(duplicatedNearest[table])=12 and List.AllTrue(List.Transform(Table.Column(duplicatedNearest[table],"dist"),each _=0)),
        InvalidNearestCountRejected=(try ops[gisNearestN](-1)[nearestCount])[HasError]
    ],
    evaluated=Record.FromList(List.Transform(Record.FieldNames(checks),(name)=>
        let result=try Record.Field(checks,name) in if result[HasError] then result[Error][Message] else result[Value]),Record.FieldNames(checks))
in if List.AllTrue(List.Transform(Record.FieldValues(evaluated),each _=true)) then evaluated
    else error Error.Record("TestFailure",Text.FromBinary(Json.FromValue(evaluated)),null)
