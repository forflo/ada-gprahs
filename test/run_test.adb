with Directed_Graph;

with Ada.Text_IO; use Ada.Text_IO;

procedure Run_Test is
   package Bool_Io is new Ada.Text_IO.Enumeration_Io(Boolean);
   use Bool_Io;

   package Int_Io is new Ada.Text_IO.Enumeration_Io(Integer);
   use Int_Io;

   package Test_Graph is new Directed_Graph(Integer, Integer);
   use Test_Graph;
   
   Graph : Graph_Data := Empty_Graph;
   V1 : Vertex;
   V2 : Vertex;
   V3 : Vertex;
   V4 : Vertex;
   E1 : Edge;
   E2 : Edge;
   E3 : Edge;
   E4 : Edge;
begin
   V1 := Add_New_Vertex(Graph, 4);
   V2 := Add_New_Vertex(Graph, 4);
   V3 := Add_New_Vertex(Graph, 4);
   V4 := Add_New_Vertex(Graph, 4);
   E1 := Add_New_Edge(Graph, V1, V2, 5);
   E2 := Add_New_Edge(Graph, V2, V3, 5);
   E3 := Add_New_Edge(Graph, V1, V3, 5);
   E4 := Add_New_Edge(Graph, V4, V3, 5);
   Dump(Graph);
   Put(Connected_Directly(Graph, V3, V2));
   New_Line;
   Put(All_Verticies(Graph)'Length);
   Put(All_Edges(Graph)'Length);
   Put(Outgoing_Edges(Graph, V3)'Length);
end Run_Test;
