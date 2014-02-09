-module(waltzdb).
-include_lib("stdlib/include/qlc.hrl").
-compile(export_all).

-record(stage,     {value=""}).
-record(line,      {id,p1=0,p2=0}).
-record(edge,      {id,type="",p1=0,p2=0,joined=false}).
-record(edgeLabel, {id=0,p1=0,p2=0,labelName="",labelId=""}).
-record(junction,  {id=0,p1,p2,p3,basePoint,type="",name="",visited=no}).
-record(label,     {id,type,name,n1,n2,n3}).
-record(illegal,   {id,basePoint,labelId}).

getX(Val) -> Val div 100.
getY(Val) -> Val rem 100.

getAngle(P1,P2,PI) ->
    DeltaX = getX(P2) - getX(P1),
    DeltaY = getY(P2) - getY(P1),
    if (DeltaX == 0) and (DeltaY > 0) -> PI/2;
       (DeltaX == 0) and (DeltaY < 0) -> PI/-2;
       (DeltaY == 0) and (DeltaX > 0) -> 0.0;
       (DeltaY == 0) and (DeltaX < 0) -> PI;
       true -> math:atan2(DeltaY,DeltaX) end.

getInscribedAngle(BasePoint,P1,P2,PI) ->
    Angle1 = getAngle(BasePoint,P1,PI),
    Angle2 = getAngle(BasePoint,P2,PI),
    Temp = Angle1 - Angle2,
    Temp2 = if Temp < 0 -> -Temp; true -> Temp end,
    Temp3 = if Temp2 > PI -> 2*PI - Temp2; true -> Temp2 end,
    if Temp3 < 0 -> -Temp3; true -> Temp3 end.

make3Junction(BasePoint,P1,P2,P3,PI) ->
    Angle12 = getInscribedAngle(BasePoint,P1,P2,PI),
    Angle13 = getInscribedAngle(BasePoint,P1,P3,PI),
    Angle23 = getInscribedAngle(BasePoint,P2,P3,PI),
    Sum1213 = Angle12 + Angle13,
    Sum1223 = Angle12 + Angle23,
    Sum1323 = Angle13 + Angle23,
    {Sum,Jun} = if (Sum1213 <  Sum1223) and (Sum1213 <  Sum1323) -> {Sum1213,#junction{p2=P1,p1=P2,p3=P3}};
                   (Sum1213 <  Sum1223) and (Sum1213 >= Sum1323) -> {Sum1323,#junction{p2=P3,p1=P1,p3=P2}};
                   (Sum1213 >= Sum1223) and (Sum1223 <  Sum1323) -> {Sum1223,#junction{p2=P2,p1=P1,p3=P3}};
                   (Sum1213 >= Sum1223) and (Sum1223 >= Sum1323) -> {Sum1323,#junction{p2=P3,p1=P1,p3=P2}} end,
    Delta = Sum - PI,
    Delta2 = if Delta < 0 -> -Delta; true -> Delta end,
    if Delta2 < 0.001 -> Jun#junction{name=tee};
       (Delta2 >= 0.001) and (Sum > PI) -> Jun#junction{name=fork};
       (Delta2 >= 0.001) and (Sum =< PI) -> Jun#junction{name=arrow} end.

reverse_edges() ->
    H = qlc:q([ begin 
        ets:delete(lines,Line),
        ets:insert(edges,#edge{id=No*100+1,p1=P1,p2=P2,joined=false}),
        ets:insert(edges,#edge{id=No*100+2,p1=P2,p2=P1,joined=false})
    end || #line{id=No,p1=P1,p2=P2}=Line <- ets:table(lines) ]),
    qlc:e(H).

detect_junctions() ->
    PI = math:pi(),
    H1 = qlc:q([ begin
        Junction = make3Junction(BP,P1,P2,P3,PI),
        J = Junction#junction{basePoint=BP,type='3j'},
        ets:insert(junctions,J),
%        io:format("Junction: ~p~n",[J]),
        ets:insert(edges,E1#edge{joined=true,type='3j'}),
        ets:insert(edges,E2#edge{joined=true,type='3j'}),
        ets:insert(edges,E3#edge{joined=true,type='3j'})
    end || E1=#edge{id=I1,joined=false,p1=BP,p2=P1} <- ets:table(edges),
           E2=#edge{id=I2,joined=false,p1=BP,p2=P2} <- ets:table(edges),
           E3=#edge{id=I3,joined=false,p1=BP,p2=P3} <- ets:table(edges),
           E2#edge.p2 /= P1, E3#edge.p2 /= P2
    ]),

    qlc:e(H1).

make_L() -> 
    H = qlc:q([ begin
        Junction = #junction{p1=P3,p2=P2,p3=no,type='2j',basePoint=BasePoint,name='L'},
%        io:format("Junction: ~p~n",[Junction]),
        ets:insert(junctions,Junction),
        ets:insert(edges,E1#edge{joined=true,type='2j'}),
        ets:insert(edges,E2#edge{joined=true,type='2j'})
    end || E1=#edge{joined=false,p1=BasePoint,p2=P2} <- ets:table(edges),
           E2=#edge{joined=false,p1=BasePoint,p2=P3} <- ets:table(edges),
           P3 /= P2, P3 /= BasePoint
    ]),

    qlc:e(H).

boundary_junction_L(L,R) ->
    [ begin

        H = qlc:q([ begin
            io:format("Boundary 2j [~p,~p]: ~p~n",[Junction,L,R]),
            ets:insert(junctions,Junction#junction{visited=yes}),
            ets:insert(edgeLabels,#edgeLabel{id=E1#edge.id,p1=BasePoint,p2=P1,labelName='B',labelId=1}),
            ets:insert(edgeLabels,#edgeLabel{id=E2#edge.id,p1=BasePoint,p2=P2,labelName='B',labelId=1})
        end || E1=#edge{p1=BP,p2=EP1} <- ets:table(edges),
               E2=#edge{p1=BP,p2=EP2} <- ets:table(edges),
               BP == BasePoint, EP1 == P1, EP2 == P2
        ]),

        C = qlc:cursor(H),
        Result=qlc:next_answers(C, 1),
        qlc:delete_cursor(C)
%       qlc:e(H)

    end || Junction=#junction{visited=no,type='2j',basePoint=BasePoint,p1=P1,p2=P2} <- ets:tab2list(junctions),
           L =< BasePoint, BasePoint =< R ].

boundary_junction_arrow(L,R) ->
    [ begin

        H = qlc:q([ begin
            io:format("Boundary 3j [~p,~p]: ~p~n",[Junction,L,R]),
            ets:insert(junctions,Junction#junction{visited=yes}),
            ets:insert(edgeLabels,#edgeLabel{id=E1#edge.id,p1=BasePoint,p2=P1,labelName='B',labelId=14}),
            ets:insert(edgeLabels,#edgeLabel{id=E2#edge.id,p1=BasePoint,p2=P2,labelName='+',labelId=14}),
            ets:insert(edgeLabels,#edgeLabel{id=E3#edge.id,p1=BasePoint,p2=P3,labelName='B',labelId=14})
        end || E1=#edge{p1=BP,p2=EP1} <- ets:table(edges),
               E2=#edge{p1=BP,p2=EP2} <- ets:table(edges),
               E3=#edge{p1=BP,p2=EP3} <- ets:table(edges),
               EP1 == P1, EP2 == P2, EP3 == P3, BasePoint == BP
        ]),

        C = qlc:cursor(H),
        Result=qlc:next_answers(C, 1),
        qlc:delete_cursor(C)
%        qlc:e(H)

    end ||
           Junction=#junction{type='3j',name=arrow,basePoint=BasePoint,p1=P1,p2=P2,p3=P3} <- ets:tab2list(junctions),
           L =< BasePoint, BasePoint =< R ].

fire_rules() ->
    reverse_edges(),
    detect_junctions(),
    Sorted = lists:sort(fun(#junction{basePoint=B1},#junction{basePoint=B2}) -> B1 < B2 end,ets:tab2list(junctions)),
    First = (hd(Sorted))#junction.basePoint,
    Last = (hd(lists:reverse(Sorted)))#junction.basePoint,
    io:format("First BP: ~p~n",[First]),
    io:format("Last BP: ~p~n",[Last]),
    make_L(),
    boundary_junction_L     (First,First),
    boundary_junction_arrow (First,First),
    boundary_junction_L     (Last,Last),
    boundary_junction_arrow (Last,Last),
    ok.

main() ->
    {ok,[Lines]} = file:consult("waltzdb16.dat"),
    ets:new(stages,     [set,named_table,{keypos,#stage.value},public]),
    ets:new(lines,      [set,named_table,{keypos,#line.id},public]),
    ets:new(edges,      [set,named_table,{keypos,#edge.id},public]),
    ets:new(junctions,  [set,named_table,{keypos,#junction.basePoint},public]),
    ets:new(edgeLabels, [set,named_table,{keypos,#edgeLabel.id},public]),
    ets:insert(stages,#stage{value=0}),
    io:format("Input: ~p~n",[length(Lines)]),
    [ ets:insert(lines,#line{id=No,p1=X,p2=Y}) 
      || {{X,Y},No} <- lists:zip(Lines,lists:seq(1,length(Lines)))],

    {T,_} = timer:tc(waltzdb,fire_rules,[]),
    io:format("Running Time: ~p ms~n",[T div 1000]),
    io:format("Lines: ~p~n",[length(ets:tab2list(lines))]),
    io:format("Edges: ~p~n",[length(ets:tab2list(edges))]),
    io:format("Junctions: ~p~n",[length(ets:tab2list(junctions))]),
    io:format("Edge Labels: ~p~n",[length(ets:tab2list(edgeLabels))]),
    [ ets:delete(T) || T <- [stages,lines,edges,junctions,edgeLabels] ],
    ok.
