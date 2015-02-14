--  pragma Profile(No_Implementation_Extensions);

with Ada.Containers.Vectors;

generic
   type Vertex_Decoration is private;
   type Edge_Decoration is private;
package Directed_Graph
is
   type Vertex_Id is private;
   type Edge_Id is private;

   type Graph_Data is private;
   Empty_Graph : constant Graph_Data;

   type Vertex is record
      Id : Vertex_Id;
      Decoration : Vertex_Decoration;
   end record;
   function "="(left : Vertex; right : Vertex) return boolean;

   type Edge is record
      Id : Edge_Id;
      Decoration : Edge_Decoration;

      Source : Vertex_Id;
      Target : Vertex_Id;
   end record;
   function "="(left : Edge; right : Edge) return boolean;

   --  function Add_New_Vertex
   --    (Into : Graph_Data;
   --     Decoration : Vertex_Decoration)
   --    return Vertex
   --  with
   --    Post => Contains(Inside => Into, Vertex => Add_New_Vertex'Result);

   --  function Add_New_Edge
   --    (Into :  Graph_Data;
   --     From : Vertex;
   --     To : Vertex;
   --     via : Edge_Decoration)
   --    return Edge
   --  with
   --    Pre => Contains(Into, Vertex => From) and
   --           Contains(Into, Vertex => To),
   --    Post => Contains(Into, Edge => Add_New_Edge'Result);

   --  function Decoration_Of
   --    (Id : Vertex;
   --     Inside : Graph_Data)
   --    return Vertex_Decoration
   --  with
   --    Pre => Contains(Inside, Id);

   --  function Decoration_Of
   --    (Id : Edge;
   --     Inside : Graph_Data)
   --    return Edge_Decoration
   --  with Pre => Contains(Inside, Id);

   type Verticies is array (Positive range <>) of Vertex;
   type Edges is array (Positive range <>) of Edge;

   --  function Neighbours
   --    (Inside : Graph_Data;
   --     Of_Vertex : Vertex)
   --    return Verticies
   --  with
   --    Pre => Contains(Inside, Of_Vertex),
   --    Post => (for all Value of Neighbours'Result =>
   --               Connected_Directly(Inside, From => Of_Vertex, To => Value));

   function Outgoing_Edges
     (Inside : Graph_Data;
      Of_Vertex : Vertex)
     return Edges
   with
     Pre => Contains(Inside, Of_Vertex),
     Post => (for all Value of Outgoing_Edges'Result =>
                Contains(Inside, Edge => Value) and
                Value.Source = Of_Vertex.Id);

   --  function Incoming_Edges
   --    (Inside : Graph_Data;
   --     Of_Vertex : Vertex)
   --    return Edges
   --  with
   --    Pre => Contains(Inside, Of_Vertex),
   --    Post => (for all Value of Incoming_Edges'Result =>
   --               Contains(Inside, Edge => Value) and
   --               Value.Target = Of_Vertex.Id);

   -- Assertion Properties
   function Contains(Inside : Graph_Data; Vertex : Directed_Graph.Vertex) return boolean;
   function Contains(Inside : Graph_Data; Edge : Directed_Graph.Edge) return boolean;

   function Connected_Directly
     (Inside : Graph_Data;
      From : Vertex;
      To : Vertex)
     return Boolean
   with
     Pre => Contains(Inside,Vertex => From) and
            Contains(Inside, Vertex => To);

private
   type Vertex_Id is new positive;
   type Edge_Id is new positive;

   type Vertex_Data is record
      First_Edge : positive;
      Vertex_Info : Vertex;
   end record;
   function "="(Left : Vertex_Data; Right : Vertex_Data) return Boolean;
   function "<"(Left : Vertex_Data; Right : Vertex_Data) return Boolean;

   package Vertex_Vectors is new Ada.Containers.Vectors
     (Index_Type => positive, Element_Type => Vertex_Data);
   package Vertex_Vectors_Sort is
      new Vertex_Vectors.Generic_Sorting;
   use Vertex_Vectors_Sort;

   function "<"(Left : Edge; Right : Edge) return Boolean;

   package Edge_Vectors is new Ada.Containers.Vectors
     (Index_Type => positive, Element_Type => Edge);
   package Edge_Vectors_Sort is
      new Edge_Vectors.Generic_Sorting;
   use Edge_Vectors_Sort;

   type Graph_Data is record
      Last_Created_Vertex : Vertex_Id;
      Last_Created_Edge : Edge_Id;

      Edge_Pointers : Vertex_Vectors.Vector;
      Edges : Edge_Vectors.Vector;
   end record
     with Dynamic_Predicate =>
         Is_Sorted(Graph_Data.Edge_Pointers) and
         Is_Sorted(Graph_Data.Edges);

   Empty_Graph : constant Graph_Data := (1,1,Vertex_Vectors.Empty_Vector, Edge_Vectors.Empty_Vector);
end Directed_Graph;
