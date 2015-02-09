--  pragma Profile(No_Implementation_Extensions);

with Ada.Containers.Vectors;

generic
   type Vertex_Decoration is private;
   type Edge_Decoration is private;
package Directed_Graph
is
   type Vertex_Id is private;
   type Edge_Id is private;
   function "="(left : Vertex_Id; right : Vertex_Id) return boolean;
   function "="(left : Edge_Id; right : Edge_Id) return boolean;

   type Graph_Data is private;

   type Vertex is record
      Id : Vertex_Id;
      Decoration : Vertex_Decoration;
   end record;

   type Edge is record
      Id : Edge_Id;
      Decoration : Edge_Decoration;

      Source : Vertex_Id;
      Target : Vertex_Id;
   end record;

   function New_Vertex
     (Into : Graph_Data;
      Decoration : Vertex_Decoration)
     return Vertex
   with
     Post => Contains(Inside => Into, Vertex => New_Vertex'Result.Id);

   function New_Edge
     (Into :  Graph_Data;
      From : Vertex_Id;
      To : Vertex_Id;
      via : Edge_Decoration)
     return Edge
   with
     Pre => Contains(Into, Vertex => From) and then
            Contains(Into, Vertex => To),
     Post => Contains(Into, Edge => New_Edge'Result.Id);

   function Decoration_Of
     (Id : Vertex_Id;
      Inside : Graph_Data)
     return Vertex_Decoration
   with
     Pre => Contains(Inside, Vertex => Id);

   function Decoration_Of
     (Id : Edge_Id;
      Inside : Graph_Data)
     return Edge_Decoration
   with Pre => Contains(Inside, Edge => Id);

   type Verticies is array (Positive) of Vertex;
   type Edges is array (Positive) of Edge;

   function Neighbours
     (Inside : Graph_Data;
      Of_Vertex : Vertex_Id)
     return Verticies
   with
     Pre => Contains(Inside, Of_Vertex),
     Post => (for all Index in Neighbours'Result'Range =>
                Contains(Inside, Vertex => Neighbours'Result(Index).Id) and then
                Connected_Directly(Inside, From => Of_Vertex, To => Neighbours'Result(Index).Id));

   function Outgoing_Edges
     (Inside : Graph_Data;
      Of_Vertex : Vertex_Id)
     return Edges
   with
     Pre => Contains(inside, vertex => of_vertex),
     Post => (for all Index in Outgoing_Edges'Result'Range =>
                Contains(Inside, Edge => Outgoing_Edges'Result(Index).Id) and then
                Outgoing_Edges'Result(Index).Source = Of_Vertex);

   function Incoming_Edges
     (Inside : Graph_Data;
      Of_Vertex : Vertex_Id)
     return Edges
   with
     Pre => Contains(Inside, Vertex => Of_Vertex),
     Post => (for all Index in Incoming_Edges'Result'Range =>
                Contains(Inside, Edge => Incoming_Edges'Result(Index).Id) and then
                Incoming_Edges'Result(Index).Target = Of_Vertex);

   -- Assertion Properties
   function Contains(Inside : Graph_Data; Vertex : Vertex_Id) return boolean;
   function Contains(Inside : Graph_Data; Edge : Edge_Id) return boolean;

   function Connected_Directly
     (Inside : Graph_Data;
      From : Vertex_Id;
      To : Vertex_Id)
     return boolean;

private
   type Vertex_Id is new positive;
   type Edge_Id is new positive;

   type Vertex_Data is record
      First_Edge : positive;
      Vertex_Info : Vertex;
   end record;

   package Vertex_Vectors is new Ada.Containers.Vectors
     (Index_Type => positive, Element_Type => Vertex_Data);

   package Edge_Vectors is new Ada.Containers.Vectors
     (Index_Type => positive, Element_Type => Edge);

   type Graph_Data is record
      Edge_Pointers : Vertex_Vectors.Vector;
      Edges : Edge_Vectors.Vector;
   end record;
end Directed_Graph;
