(* ::Package:: *)

BeginPackage["Motion`Acclaim`"];


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


Clear[Motion`Acclaim`Private`DropComments];
Motion`Acclaim`Private`DropComments[raw_String] :=
  StringTrim@StringReplacePart[
   raw, 
   "", 
   StringPosition[
    raw,
    Shortest[StringExpression["#" ~~ ___ ~~ EndOfLine]]
    ]
   ];


Clear[Motion`Acclaim`Private`SplitBlocks];
Motion`Acclaim`Private`SplitBlocks[raw_String] := Association@Map[
   #[[1]] -> #[[2 ;;]] &,
    Map[StringTrim /@ StringSplit[#, "\n"] &, StringSplit[raw, ":"]]
   ];


Clear[Motion`Acclaim`Private`ParseRoot];
Motion`Acclaim`Private`ParseRoot[raw_List] :=
  Block[
   {
    blocks,
    
    position,
    orientation,
    
    result
    },
   blocks = Association@Map[
      #[[1]] -> #[[2 ;;]] &,
      StringSplit[#, Whitespace] & /@ raw
      ];
   
   position = Internal`StringToDouble /@ blocks["position"];
   orientation = Internal`StringToDouble /@ blocks["position"];
   
   result = <|
     "position" -> position,
     "orientation" -> orientation
     |>;
   
   result
   ];


Clear[Motion`Acclaim`Private`ParseBones];
Motion`Acclaim`Private`ParseBones[raw_List] := Block[
  {
   blocks,
   result
   },
   blocks = Map[
    Association[Map[#[[1]] -> #[[2 ;;]] &, StringSplit[#]]] &,
    Map[
     raw[[#[[1]] + 1 ;; #[[2]] - 1]] &,
     Transpose[{
       Flatten[Position[raw, "begin"]],
       Flatten[Position[raw, "end"]]
       }]
     ]
    ];
  result = Association@Map[
     #["name"] -> # &,
     Map[
      Block[
        {
         id = #["id"][[1]],
         name = #["name"][[1]],
         direction = Internal`StringToDouble /@ #["direction"],
         length = #["length"][[1]],
         axis = {
           Internal`StringToDouble[#]  & /@ #[
              "axis"][[1 ;; 3]],
           #["axis"][[4]]
           },
         dof = If[
           KeyExistsQ[#, "dof"],
           {
            If[MemberQ[#["dof"], "rx"], 
             Position[#["dof"], "rx"][[1, 1]], -1],
            
            If[MemberQ[#["dof"], "ry"], 
             Position[#["dof"], "ry"][[1, 1]], -1],
            
            If[MemberQ[#["dof"], "rz"], 
             Position[#["dof"], "rz"][[1, 1]], -1]
            },
           {-1, -1, -1}
           ]
         },
        <|
         "id" -> id,
         "name" -> name,
         "direction" -> direction,
         "length" -> Internal`StringToDouble[length],
         "axis" -> axis,
         "dof" -> dof
         |>
        ] &,
      blocks
      ]
     ];
  
  result
  ];


Clear[Motion`Acclaim`Private`ParseHierarchy];
Motion`Acclaim`Private`ParseHierarchy[raw_List] :=
  Block[
   {
    paths = StringSplit /@ raw[[2 ;; -2]],
    v,
    e,
    result
    },
   v = DeleteDuplicates@Flatten[paths];
   e = Flatten@Map[
      Block[
        {
         parent = #[[1]], 
         children = #[[2 ;;]]
         },
        parent -> # & /@ children
        ] &,
      paths
      ];
   
   result = Association@Map[
     Block[
       {
        path,
        parent
        },
       path = FindShortestPath[Graph[e], "root", #];
       parent = If[Length[path] > 1, path[[-2]], Null];
       # -> <|
         "path" -> path,
         "parent" -> parent
         |>
       ] &,
     v
     ];

   result
   ];


End[];


(* ::Section:: *)
(*Public*)


Clear[Motion`Acclaim`ImportAFS];
Motion`Acclaim`ImportAFS[raw_] := Block[
   {
    str = raw,
    blocks,
    
    root,
    bones,
    hierarchy,
    
    result
    },
   str = Motion`Acclaim`Private`DropComments[str];
   blocks = Motion`Acclaim`Private`SplitBlocks[str];
   root = Motion`Acclaim`Private`ParseRoot[blocks["root"]];
   bones = Motion`Acclaim`Private`ParseBones[blocks["bonedata"]];
   hierarchy = Motion`Acclaim`Private`ParseHierarchy[blocks["hierarchy"]];
   
   result = <|
     "root" -> root,
     "bones" -> bones,
     "hierarchy" -> hierarchy
     |>;
   
   result
   ];


EndPackage[];
