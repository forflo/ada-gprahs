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

   function Add_New_Vertex
     (Into : in out Graph_Data;
      Decoration : Vertex_Decoration)
     return Vertex
   with
     Post => Contains(Into, Vertex => Add_New_Vertex'Result);

   function Add_New_Edge
     (Into : in out Graph_Data;
      From : Vertex;
      To : Vertex;
      Via : Edge_Decoration)
     return Edge
   with
     Pre => Contains(Into, Vertex => From) and
     Contains(Into, Vertex => To),
     Post => Contains(Into, Edge => Add_New_Edge'Result);

   type Verticies is array (Positive range <>) of Vertex;
   type Edges is array (Positive range <>) of Edge;

   function All_Verticies(Inside : Graph_Data) return Verticies;
   function All_Edges(Inside : Graph_Data) return Edges;

   function Neighbours
     (Inside : Graph_Data;
      Of_Vertex : Vertex)
     return Verticies
   with
     Pre => Contains(Inside, Of_Vertex),
     Post => (for all Value of Neighbours'Result =>
                Connected_Directly(Inside, From => Of_Vertex, To => Value));

   function Outgoing_Edges
     (Inside : Graph_Data;
      Of_Vertex : Vertex)
     return Edges
   with
     Pre => Contains(Inside, Of_Vertex),
     Post => (for all Value of Outgoing_Edges'Result =>
                Contains(Inside, Edge => Value) and
                Value.Source = Of_Vertex.Id);
   
   function Incoming_Edges
     (Inside : Graph_Data;
      Of_Vertex : Vertex)
     return Edges
   with
     Pre => Contains(Inside, Of_Vertex),
     Post => (for all Value of Incoming_Edges'Result =>
                Contains(Inside, Edge => Value) and
                Value.Target = Of_Vertex.Id);

   -- Assertion Properties
   function Contains(Inside : Graph_Data; Vertex : Directed_Graph.Vertex) return Boolean;
   function Contains(Inside : Graph_Data; Edge : Directed_Graph.Edge) return Boolean;

   function Connected_Directly
     (Inside : Graph_Data;
      From : Vertex;
      To : Vertex)
     return Boolean
   with
     Pre => Contains(Inside,Vertex => From) and
     Contains(Inside, Vertex => To);

   -- TODO: should this guy be in a child package?
   -- Debugging
   procedure Dump(Graph : Graph_Data);

private

   type Vertex_Id is new Positive;
   subtype Maybe_Vertex_Id is Vertex_ID'Base
     range 0..Vertex_Id'Last;
   No_Vertex_Id : constant Maybe_Vertex_Id := 0;

   type Edge_Id is new Positive;
   subtype Maybe_Edge_Id is Edge_ID'Base
     range 0..Edge_Id'Last;
   No_Edge_Id : constant Maybe_Edge_Id := 0;

   subtype Index is Positive;
   subtype Maybe_Index is Index'Base
     range 0..Index'Last;
   No_Index : constant Maybe_Index := 0;

   -- TODO: To reduce memory usage use an accumulative sum
   type Vertex_Data is record
      Vertex : Directed_Graph.Vertex;

      First_Outgoing_Edge : Maybe_Index;
      Outgoing_Edges : Natural;

      First_Incoming_Edge : Maybe_Index;
      Incoming_Edges : Natural;
   end record;

   function "="(Left : Vertex_Data; Right : Vertex_Data) return Boolean;
   function "<"(Left : Vertex_Data; Right : Vertex_Data) return Boolean;


   type Edge_Data is record
      Edge : Directed_Graph.Edge;
      Next_Incoming : Maybe_Index;
   end record;

   function "="(Left : Edge_Data; Right : Edge_Data) return Boolean;
   function "<"(Left : Edge_Data; Right : Edge_Data) return Boolean;



   package Vertex_Vectors is new Ada.Containers.Vectors
     (Index_Type => Index, Element_Type => Vertex_Data);

   package Vertex_Vectors_Sort is
     new Vertex_Vectors.Generic_Sorting;

   use Vertex_Vectors_Sort;


   package Edge_Vectors is new Ada.Containers.Vectors
     (Index_Type => Index, Element_Type => Edge_Data);

   package Edge_Vectors_Sort is
     new Edge_Vectors.Generic_Sorting;

   use Edge_Vectors_Sort;



   type Graph_Data is record
      Last_Created_Vertex : Maybe_Vertex_Id;
      Last_Created_Edge : Maybe_Edge_Id;

      Edge_Pointers : Vertex_Vectors.Vector;
      Edges : Edge_Vectors.Vector;
   end record;
   --  with Dynamic_Predicate =>
   --    Is_Sorted(Graph_Data.Edge_Pointers) and
   --    Is_Sorted(Graph_Data.Edges);

   Empty_Graph : constant Graph_Data :=
     (Last_Created_Vertex => No_Vertex_Id,
      Last_Created_Edge => No_Edge_Id,
      Edge_Pointers => Vertex_Vectors.Empty_Vector,
      Edges => Edge_Vectors.Empty_Vector);

end Directed_Graph;
