with Vector_Utils;
use Vector_Utils;

package body Directed_Graph
is
   function "="(Left : Vertex; Right : Vertex) return Boolean is
      (Left.Id = Right.Id);

   function "="(Left : Edge; Right : Edge) return Boolean is
      (Left.Source = Right.Source and Left.Id = Right.Id);

   function "<"(Left : Edge; Right : Edge) return Boolean is
      (if Left.Source.Id < Right.Source.Id then Left.Id < Right.Id);

   function "="(Left : Vertex_Data; Right : Vertex_Data) return Boolean is
      (Left.Vertex_Info = Right.Vertex_Info);

   function "<"(Left : Vertex_Data; Right : Vertex_Data) return Boolean is
      (Left.Vertex_Info.Id < Right.Vertex_Info.Id);

   function Contains_Any_Matching is
      new Vector_Utils.Contains_Any_Matching(Vertex_Vectors);

   function Contains_Any_Matching is
      new Vector_Utils.Contains_Any_Matching(Edge_Vectors);

   function Contains(Inside : Graph_Data; Vertex : Directed_Graph.Vertex) return Boolean is
      function IsThisVertex(Vert : Vertex_Data) return Boolean is
      begin
         return Vert.Vertex_Info.Id = Vertex.Id;
      end IsThisVertex;
   begin
      return Contains_Any_Matching(Inside.Edge_Pointers, IsThisVertex'Access);
   end Contains;

   function Contains(Inside : Graph_Data; Edge : Directed_Graph.Edge) return Boolean is
      function IsThisEdge(Other_Edge : Directed_Graph.Edge) return Boolean is
      begin
         return Other_Edge = Edge;
      end IsThisEdge;
   begin
      return Contains_Any_Matching(Inside.Edges, IsThisEdge'Access);
   end Contains;

   function Connected_Directly
     (Inside : Graph_Data;
      From : Vertex;
      To : Vertex)
     return Boolean is
      (for some Edge of Inside.Edges =>
         Edge.Source.Id = From.Id and Edge.Target.Id = To.Id);

   procedure Length_And_Index_Out
     (Inside : Graph_Data;
      Of_Vertex : Vertex;
      Length : out Natural;
      Edge_Index : out Positive)
   is
      function Judge(Data : Vertex_Data) return Search_Hint
      is
         Id : constant Vertex_Id := Data.Vertex_Info.Id;
      begin
         if Id < Of_Vertex.Id then
            return Bigger;
         elsif Of_Vertex.Id < Id then
            return Smaller;
         else
            return Correct;
         end if;
      end Judge;

      function Find is new Find_Matching(Vertex_Vectors, Judge);

      Start_Edge_Index : Positive;
      Next_Edge_Index : Positive;
      Data : Vertex_Data;
      Data_Index : Positive;
      Proper_Hint : Vertex_Vectors.Extended_Index;
   begin
      -- We try to Pick up the Vertex at it's ID, because it's quite likley that it's
      -- there when deletions of verticies are rare.
      -- So it's a good idea to start loooking there, this will likley reduce the
      -- Number of iterations for the Binary_Search invloved
      -- TODO: As of now it does not take additional Information like
      -- number of deletions into account.
      -- In that case a small linear search could be faster.
      -- But it's unclear when exactly this is the case or when the Proper_Hint
      -- Becomes useless or even harmful.
      Proper_Hint :=
        (if Positive(Of_Vertex.Id) <= Inside.Edge_Pointers.Last_Index
           then Positive(Of_Vertex.Id)
           else Vertex_Vectors.No_Index);

      Data_Index := Find
        (Container => Inside.Edge_Pointers,
         Index_Hint => Proper_Hint);

      Data := Inside.Edge_Pointers(Data_Index);

      Start_Edge_Index := Data.First_Edge;
      Next_Edge_Index :=
        (if Data_Index + 1 <= Inside.Edge_Pointers.Last_Index
           then Inside.Edge_Pointers(Data_Index + 1).First_Edge
           else Start_Edge_Index);

      Length := Next_Edge_Index - Start_Edge_Index;
      Edge_Index := Start_Edge_Index;
   end Length_And_Index_Out;

   function Outgoing_Edges
     (Inside : Graph_Data;
      Of_Vertex : Vertex)
     return Edges
   is
      Length : Natural;
      First_Edge : Positive;

   begin
      Length_And_Index_Out(Inside,Of_Vertex, Length, First_Edge);
      declare
         Result : Edges(Positive range 1..Length);
      begin
         for Index in First_Edge..(First_Edge + Length) loop
            Result(Index) := Inside.Edges(Index);
         end loop;
         return Result;
      end;
   end Outgoing_Edges;

end Directed_Graph;
