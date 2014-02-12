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
        ets:insert(edges,#edge{id=No*100+2,p1=P2,p2=P1,joined=false}),
        ok
    end || #line{id=No,p1=P1,p2=P2}=Line <- ets:table(lines) ]),
    qlc:e(H).

detect_junctions() ->
    PI = math:pi(),
    H1 = qlc:q([ begin if (BP2 == BP3) ->
        Junction = make3Junction(BP1,P1,P2,P3,PI),
        J = Junction#junction{basePoint=BP1,type='3j'},
        ets:insert(junctions,J),
%        io:format("Junction: ~p~n",[J]),
        ets:insert(edges,E1#edge{joined=true,type='3j'}),
        ets:insert(edges,E2#edge{joined=true,type='3j'}),
        ets:insert(edges,E3#edge{joined=true,type='3j'});
        true -> skip end
    end || E1=#edge{id=I1,joined=false,p1=BP1,p2=P1} <- ets:table(edges),
           E2=#edge{id=I2,joined=false,p1=BP2,p2=P2} <- ets:table(edges),
           E3=#edge{id=I3,joined=false,p1=BP3,p2=P3} <- ets:table(edges),
           P2 /= P1, P3 /= P2, BP1 == BP2
    ]),

    qlc:e(H1).



make_L() -> 
    H = qlc:q([ begin

        H1 = qlc:q([ ok || E=#edge{p1=EP1,p2=EP2} <- ets:table(edges),
               EP1 == BP, EP2 /= P2, EP2 /= P3

        ]),

        C = qlc:cursor(H1),
        Result=qlc:next_answers(C, 1),
        qlc:delete_cursor(C),

        if Result == [] ->
            Junction = #junction{p1=P2,p2=P3,p3=0,type='2j',basePoint=BP,name='L'},
%            io:format("Junction: ~p~n",[Junction]),
            ets:insert(junctions,Junction),
            ets:insert(edges,E1#edge{joined=true,type='2j'}),
            ets:insert(edges,E2#edge{joined=true,type='2j'}),
            ok;

           true -> skip end

    end || E1=#edge{joined=false,p1=BP,p2=P2} <- ets:table(edges),
           E2=#edge{joined=false,p1=E2P1,p2=P3} <- ets:table(edges),
           E2P1 == BP, P3 /= P2
    ]),

    qlc:e(H).

boundary_junction_L(L,R) ->
    [ begin

        H = qlc:q([ begin
            io:format("Boundary 2j: ~p~n",[Junction]),
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

    end || Junction=#junction{type='2j',basePoint=BasePoint,p1=P1,p2=P2,p3=P3} <- ets:tab2list(junctions),
           L =< BasePoint, BasePoint =< R,P3==0 ].

boundary_junction_arrow(L,R) ->
    [ begin

        H = qlc:q([ begin
            io:format("Boundary 3j: ~p~n",[Junction]),
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
           Junction=#junction{type='3j',name=arrow,basePoint=BasePoint,p1=P1,p2=P2,p3=P3} <- 
            ets:tab2list(junctions),
           L =< BasePoint, BasePoint =< R ].

visit_3j() ->
    H = qlc:q([ begin
        Junction = J#junction{visited=now},
        io:format("Junction: ~p~n",[Junction]),
        ets:insert(junctions,Junction)
    end || J=#junction{visited=no,type='3j'} <- ets:table(junctions)
    ]),

    qlc:e(H).

visit_3j_0() ->
    H = qlc:q([ begin
        io:format("Labeling 3j 0: ~p~n",[{L,J}]),
        ets:insert(edgeLabels,E1#edgeLabel{p1=BP,p2=P1,labelName=N1,labelId=Id}),
        ets:insert(edgeLabels,E2#edgeLabel{p1=BP,p2=P2,labelName=N2,labelId=Id}),
        ets:insert(edgeLabels,E3#edgeLabel{p1=BP,p2=P3,labelName=N3,labelId=Id}),
        ok
    end || J=#junction{visited=now,name=Name,basePoint=BP,p1=P1,p2=P2,p3=P3} <- ets:table(junctions),
           L=#label{n1=N1,n2=N2,n3=N3,name=Name2,id=Id} <- ets:table(labels),
           E1=#edgeLabel{p1=AP1,p2=ABP,labelName=AN1} <- ets:table(edgeLabels),
           E2=#edgeLabel{p1=AP2,p2=ABP1,labelName=AN2} <- ets:table(edgeLabels),
           E3=#edgeLabel{p1=AP3,p2=ABP2,labelName=AN3} <- ets:table(edgeLabels),
           Name == Name2, ABP == BP, ABP1 == BP, ABP2 == BP,
           AP1 == P1, AP2 == P2, AP3 == P3,
           AN1 == N1, AN2 == N2, AN3 == N3
    ]),

    qlc:e(H).

visit_3j_1() ->
    H = qlc:q([ begin
        io:format("Labeling 3j 1: ~p~n",[{L,J}]),
        ets:insert(edgeLabels,E2#edgeLabel{p1=BP,p2=P2,labelName=N2,labelId=Id}),
        ets:insert(edgeLabels,E3#edgeLabel{p1=BP,p2=P3,labelName=N3,labelId=Id}),
        ok
    end || J=#junction{visited=now,name=Name,basePoint=BP,p1=P1,p2=P2,p3=P3} <- ets:table(junctions),
           L=#label{n1=N1,n2=N2,n3=N3,name=Name2,id=Id} <- ets:table(labels),
           E2=#edgeLabel{p1=AP2,p2=ABP,labelName=AN2} <- ets:table(edgeLabels),
           E3=#edgeLabel{p1=AP3,p2=ABP,labelName=AN3} <- ets:table(edgeLabels),
           Name == Name2, ABP == BP,
           AP2 == P2, AP3 == P3,
           AN2 == N2, AN3 == N3
    ]),

    qlc:e(H).

visit_3j_2() ->
    H = qlc:q([ begin
        io:format("Labeling 3j 2: ~p~n",[{L,J}]),
        ets:insert(edgeLabels,E1#edgeLabel{p1=BP,p2=P1,labelName=N1,labelId=Id}),
        ets:insert(edgeLabels,E3#edgeLabel{p1=BP,p2=P3,labelName=N3,labelId=Id}),
        ok
    end || J=#junction{visited=now,name=Name,basePoint=BP,p1=P1,p2=P2,p3=P3} <- ets:table(junctions),
           L=#label{n1=N1,n2=N2,n3=N3,name=Name2,id=Id} <- ets:table(labels),
           E1=#edgeLabel{p1=AP1,p2=ABP,labelName=AN1} <- ets:table(edgeLabels),
           E3=#edgeLabel{p1=AP3,p2=ABP,labelName=AN3} <- ets:table(edgeLabels),
           Name == Name2, ABP == BP,
           AP1 == P1, AP3 == P3,
           AN1 == N1, AN3 == N3
    ]),

    qlc:e(H).

visit_3j_3() ->
    H = qlc:q([ begin
        io:format("Labeling 3j 3: ~p~n",[{L,J}]),
        ets:insert(edgeLabels,E3#edgeLabel{p1=BP,p2=P3,labelName=N3,labelId=Id}),
        ok
    end || J=#junction{visited=now,name=Name,basePoint=BP,p1=P1,p2=P2,p3=P3} <- ets:table(junctions),
           L=#label{n1=N1,n2=N2,n3=N3,name=Name2,id=Id} <- ets:table(labels),
%           E1=#edgeLabel{p1=AP1,p2=ABP,labelName=AN1} <- ets:table(edgeLabels),
%           E2=#edgeLabel{p1=AP2,p2=ABP,labelName=AN2} <- ets:table(edgeLabels),
           E3=#edgeLabel{p1=AP3,p2=ABP,labelName=AN3} <- ets:table(edgeLabels),
           Name == Name2, ABP == BP,
           AP3 == P3, %AP1 /= P1, AP2 /= P2,
           AN3 == N3%, AN1 /= N1, AN2 /= N2
    ]),

    qlc:e(H).

visit_3j_4() ->
    H = qlc:q([ begin
        io:format("Labeling 3j 4: ~p~n",[{L,J}])
    end || J=#junction{visited=now,name=Name,basePoint=BP,p1=P1,p2=P2,p3=P3} <- ets:table(junctions),
           L=#label{n1=N1,n2=N2,n3=N3,name=Name2,id=Id} <- ets:table(labels),
           E1=#edgeLabel{p1=AP1,p2=ABP,labelName=AN1} <- ets:table(edgeLabels),
           E2=#edgeLabel{p1=AP2,p2=ABP,labelName=AN2} <- ets:table(edgeLabels),
           Name == Name2, ABP == BP,
           AP1 == P1, AP2 == P2,
           AN1 == N1, AN2 == N2
    ]),

    qlc:e(H).

visit_3j_5() ->
    H = qlc:q([ begin
        io:format("Labeling 3j 5: ~p~n",[{L,J}])
    end || J=#junction{visited=now,name=Name,basePoint=BP,p1=P1,p2=P2,p3=P3} <- ets:table(junctions),
           L=#label{n1=N1,n2=N2,n3=N3,name=Name2,id=Id} <- ets:table(labels),
           E2=#edgeLabel{p1=AP2,p2=ABP,labelName=AN2} <- ets:table(edgeLabels),
           Name == Name2, ABP == BP,
           AP2 == P2,
           AN2 == N2
    ]),

    qlc:e(H).

visit_3j_6() ->
    H = qlc:q([ begin
        io:format("Labeling 3j 6: ~p~n",[{L,J}])
    end || J=#junction{visited=now,name=Name,basePoint=BP,p1=P1,p2=P2,p3=P3} <- ets:table(junctions),
           L=#label{n1=N1,n2=N2,n3=N3,name=Name2,id=Id} <- ets:table(labels),
           E1=#edgeLabel{p1=AP1,p2=ABP,labelName=AN1} <- ets:table(edgeLabels),
           Name == Name2, ABP == BP,
           AP1 == P1,
           AN1 == N1
    ]),

    qlc:e(H).

fire_rules() ->
    reverse_edges(),
    make_L(),
    detect_junctions(),
    Sorted = lists:sort(fun(#junction{basePoint=B1},#junction{basePoint=B2}) -> B1 < B2 end,ets:tab2list(junctions)),
    First = (hd(Sorted))#junction.basePoint,
    Last = (hd(lists:reverse(Sorted)))#junction.basePoint,
    io:format("First BP: ~p~n",[First]),
    io:format("Last BP: ~p~n",[Last]),
    boundary_junction_arrow (First,First),
    boundary_junction_L     (First,First),
    boundary_junction_arrow (Last,Last),
    boundary_junction_L     (Last,Last),
    visit_3j(),
    visit_3j_0(),
    visit_3j_1(),
    visit_3j_2(),
    visit_3j_3(),
    visit_3j_4(),
    visit_3j_5(),
    visit_3j_6(),
    ok.

main() ->
    {ok,[Lines]} = file:consult("waltzdb4.dat"),
    ets:new(stages,     [set,named_table,{keypos,#stage.value},public]),
    ets:new(lines,      [set,named_table,{keypos,#line.id},public]),
    ets:new(edges,      [set,named_table,{keypos,#edge.id},public]),
    ets:new(labels,     [set,named_table,{keypos,#label.id},public]),
    ets:new(junctions,  [set,named_table,{keypos,#junction.basePoint},public]),
    ets:new(edgeLabels, [set,named_table,{keypos,#edgeLabel.id},public]),
    ets:insert(stages,#stage{value=0}),
    [ ets:insert(labels,L) || L=#label{} <- Lines],
    io:format("Input: ~p~n",[length(Lines)]),
    [ ets:insert(lines,#line{id=No,p1=X,p2=Y}) 
      || {{X,Y},No} <- lists:zip(Lines,lists:seq(1,length(Lines)))],

    {T,_} = timer:tc(waltzdb,fire_rules,[]),
    io:format("Running Time: ~p ms~n",[T div 1000]),
    io:format("Labels: ~p~n",[length(ets:tab2list(labels))]),
    io:format("Lines: ~p~n",[length(ets:tab2list(lines))]),
    io:format("Edges: ~p~n",[length(ets:tab2list(edges))]),
    io:format("Junctions: ~p~n",[length(ets:tab2list(junctions))]),
    io:format("Edge Labels: ~p~n",[length(ets:tab2list(edgeLabels))]),
    [ ets:delete(T) || T <- [stages,lines,edges,junctions,edgeLabels,labels] ],
    ok.
