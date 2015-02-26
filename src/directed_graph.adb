with Vector_Utils;
use Vector_Utils;

with Ada.Containers;
use Ada.Containers;

with Ada.Text_IO; use Ada.Text_IO;

package body Directed_Graph
is
   package Integer_IO is new Ada.Text_IO.Enumeration_IO(Integer);
   use Integer_IO;

   package Edge_Id_IO is new Ada.Text_IO.Enumeration_IO(Maybe_Edge_Id);
   use Edge_Id_IO;

   package Vertex_Id_IO is new Ada.Text_IO.Enumeration_IO(Maybe_Vertex_Id);
   use Vertex_Id_IO;

   function "="(Left : Vertex; Right : Vertex) return Boolean is
      (Left.Id = Right.Id);

   function "="(Left : Edge; Right : Edge) return Boolean is
      (Left.Id = Right.Id);

   function "="(Left : Vertex_Data; Right : Vertex_Data) return Boolean is
      (Left.Vertex = Right.Vertex);

   function "<"(Left : Vertex_Data; Right : Vertex_Data) return Boolean is
      (Left.Vertex.Id < Right.Vertex.Id);

   function "="(Left : Edge_Data; Right : Edge_Data) return Boolean is
      (Left.Edge = Right.Edge);

   function "<"(Left : Edge_Data; Right : Edge_Data) return Boolean is
     (if Left.Edge.Source = Right.Edge.Source then Left.Edge.Id < Right.Edge.Id else False);

   function Find_Vertex_Data_Index
     (Inside : Graph_Data;
      Id : Vertex_Id)
      return Index
      with
        --  Pre => Contains(Inside, Id),
        Post => Inside.Edge_Pointers(Find_Vertex_Data_Index'Result).Vertex.Id = Id
   is
      function Find is new Find_Matching(Vertex_Vectors);

      function Judge(Data : Vertex_Data) return Search_Hint
      is
         Other_Id : constant Vertex_Id := Data.Vertex.Id;
      begin
         if Other_Id < Id then
            return Bigger;
         elsif Id < Other_Id then
            return Smaller;
         else
            return Correct;
         end if;
      end Judge;

      Data_Index : Index;
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
        (if Index(Id) <= Inside.Edge_Pointers.Last_Index
           then Index(Id)
           else Vertex_Vectors.No_Index);

      Data_Index := Find
        (Container => Inside.Edge_Pointers,
         Judge => Judge'access,
         Index_Hint => Proper_Hint);

      return Data_Index;
   end Find_Vertex_Data_Index;

   function Add_New_Vertex
     (Into : in out Graph_Data;
      Decoration : Vertex_Decoration)
     return Vertex
   is
      New_Id : constant Vertex_Id := Into.Last_Created_Vertex + 1;
      New_Vertex : constant Vertex :=
        (Id => New_Id,
         Decoration => Decoration);

      New_Vertex_Data : constant Vertex_Data :=
        (First_Outgoing_Edge => No_Index,
         Outgoing_Edges => 0,
         First_Incoming_Edge => No_Index,
         Incoming_Edges => 0,
         Vertex => New_Vertex);
   begin
      Into.Edge_Pointers.Append(New_Vertex_Data);
      Into.Last_Created_Vertex := New_Id;

      return New_Vertex;
   end Add_New_Vertex;

   function Add_New_Edge
     (Into : in out Graph_Data;
      From : Vertex;
      To : Vertex;
      via : Edge_Decoration)
     return Edge
   is
      New_Id : constant Edge_Id := Into.Last_Created_Edge + 1;

      Source_Index : constant Index := Find_Vertex_Data_Index(Into, From.Id);
      Target_Index : constant Index := Find_Vertex_Data_Index(Into, To.Id);

      Source_Data : constant Vertex_Data := Into.Edge_Pointers(Source_Index);
      Target_Data : constant Vertex_Data := Into.Edge_Pointers(Target_Index);

      New_Edge : constant Edge :=
        (Id => New_Id,
         Decoration => via,
         Source => From.Id,
         Target => To.Id);
      -- we can only determine the Next_Incoming
      -- once we looked ad the target Vertex_Data
      New_Edge_Data : Edge_Data :=
        (Edge => New_Edge,
         Next_Incoming => No_Index);

      -- we can only determine the new position
      -- after inspecting the source meta data
      New_Edge_Index : Index;

      -- Updates the source meta data after the insertion
      -- CAUTION: this procedure relies New_Edge_Index
      -- to be calculated (and set) already
      procedure Update_Source(Data : in out Vertex_Data)
      with
        Global => New_Edge_Index,
        Depends => (Data => New_Edge_Index)
      is
      begin
         if Data.First_Outgoing_Edge = No_Index then
            Data.First_Outgoing_Edge := New_Edge_Index;
         end if;

         Data.Outgoing_Edges := Data.Outgoing_Edges + 1;
      end;

      -- Updates the target meta data after the insertion
      -- CAUTION: this procedure relies New_Edge_Index
      -- to be calculated (and set) already
      procedure Update_Target(Data : in out Vertex_Data)
      with
        Global => New_Edge_Index,
        Depends => (Data => New_Edge_Index)
      is
      begin
         -- no incoming edges yet
         if Data.First_Incoming_Edge = No_Index then
            -- set the list head to the newly created edge
            Data.First_Incoming_Edge := New_Edge_Index;
            Data.Incoming_Edges := 1;
            -- there are already some coming edges
         else
            -- make the edge pointer point to the new head
            Data.First_Incoming_Edge := New_Edge_Index;
            Data.Incoming_Edges := Data.Incoming_Edges + 1;
         end if;
      end;

      -- Corrects the indexes in the meta data after insertion
      -- CAUTION: this procedure relies New_Edge_Index
      -- to be calculated (and set) already
      procedure Bump_Pointers(Data : in out Vertex_Data)
      with
        Global => New_Edge_Index,
        Depends => (Data => New_Edge_Index)
      is
      begin
         -- Bump all references that point after the
         -- freshly inserted Edge
         if Data.First_Outgoing_Edge >= New_Edge_Index then
            Data.First_Outgoing_Edge := Data.First_Outgoing_Edge + 1;
         end if;

         if Data.First_Incoming_Edge >= New_Edge_Index then
            Data.First_Incoming_Edge := Data.First_Incoming_Edge + 1;
         end if;
      end;

   begin
      Into.Last_Created_Edge := New_Id;

      -- We just append the new edge.
      -- this is the fast, nice and easy case
      if Source_Data.First_Outgoing_Edge = No_Index then

         -- we will append, thus the no matter
         -- where Target_Data.First_Incoming_Edge points to
         -- it will be unaffected by the insertion
         New_Edge_Data.Next_Incoming :=
           Target_Data.First_Incoming_Edge;

         Edge_Vectors.Append(Into.Edges, New_Edge_Data);

         -- Can't be No_Index
         New_Edge_Index := Into.Edges.Last_Index;

      -- we have to insert the new edge
      -- somewhere in the middle, so we have to watch out for
      -- changing indexes
      else
         -----------------------------------------------------------------------
         -- e1, e2 have v1 as source e3 has v2 as source, we insert e4
         -- e4 to has v1 as source
         -- v1, v2, v3, v4 : Vertex_Data
         -- e1, e2, e3 : Edge_Data
         --        ^ insertion point for e4, we insert before e3
         -- ^ v1.First_Outgoing_Edge
         -- v1.Outgoing_Edges = 2
         -----------------------------------------------------------------------
         New_Edge_Index :=
           Source_Data.First_Outgoing_Edge +
           Source_Data.Outgoing_Edges;

         -- the Target_Data.First_Incoming_Edge will be off by one
         -- when we insert the edge somewhere before it
         -- so we have to fix that
         New_Edge_Data.Next_Incoming :=
           (if Target_Data.First_Incoming_Edge >= New_Edge_Index
            then Target_Data.First_Incoming_Edge + 1
            else Target_Data.First_Incoming_Edge);

         Edge_Vectors.Insert
           (Container => Into.Edges,
            Before => New_Edge_Index,
            New_Item => New_Edge_Data);

         -- after inserting we have to adjust the indexes to all
         -- edges after this one
         for Cursor in Into.Edge_Pointers.Iterate loop
            Vertex_Vectors.Update_Element
              (Container => Into.Edge_Pointers,
               Position => Cursor,
               Process => Bump_Pointers'Access);
         end loop;
      end if;

      -- Update Source meta data
      Vertex_Vectors.Update_Element
        (Container => Into.Edge_Pointers,
         Index => Source_Index,
         Process => Update_Source'Access);

      -- Update Target meta data
      Vertex_Vectors.Update_Element
        (Container => Into.Edge_Pointers,
         Index => Target_Index,
         Process => Update_Target'Access);

      return New_Edge;
   end Add_New_Edge;

   function All_Verticies(Inside : Graph_Data) return Verticies
   is
      -- TODO: is this cast sound?
      Num_Verticies : constant Natural := Natural(Inside.Edge_Pointers.Length);
      Result : Verticies(Positive range 1..Num_Verticies);
      Result_Index : Positive := 1;
   begin
      for Value of Inside.Edge_Pointers loop
         Result(Result_Index) := Value.Vertex;
         Result_Index := Result_Index + 1;
      end loop;

      return Result;
   end;

   function All_Edges(Inside : Graph_Data) return Edges
   is
      -- TODO: is this cast sound?
      Num_Edges : constant Natural := Natural(Inside.Edges.Length);
      Result : Edges(Positive range 1..Num_Edges);
      Result_Index : Positive := 1;
   begin
      for Value of Inside.Edges loop
         Result(Result_Index) := Value.Edge;
         Result_Index := Result_Index + 1;
      end loop;

      return Result;
   end;

   function Contains_Any_Matching is
     new Vector_Utils.Contains_Any_Matching(Vertex_Vectors);

   function Contains_Any_Matching is
      new Vector_Utils.Contains_Any_Matching(Edge_Vectors);

   function Contains(Inside : Graph_Data; Vertex : Directed_Graph.Vertex) return Boolean is
      function IsThisVertex(Data : Vertex_Data) return Boolean is
      begin
         return Data.Vertex.Id = Vertex.Id;
      end IsThisVertex;
   begin
      return Contains_Any_Matching(Inside.Edge_Pointers, IsThisVertex'Access);
   end Contains;

   function Contains(Inside : Graph_Data; Edge : Directed_Graph.Edge) return Boolean is
      function IsThisEdge(Data : Edge_Data) return Boolean is
      begin
         return Data.Edge.Id = Edge.Id;
      end IsThisEdge;
   begin
      return Contains_Any_Matching(Inside.Edges, IsThisEdge'Access);
   end Contains;

   -- TODO: this can be done more efficiently
   -- simply use the From to find the meta data
   -- then we know exactly where to look
   function Connected_Directly
     (Inside : Graph_Data;
      From : Vertex;
      To : Vertex)
     return Boolean is
      (for some Data of Inside.Edges =>
         Data.Edge.Source = From.Id and Data.Edge.Target = To.Id);


   function Outgoing_Edges
     (Inside : Graph_Data;
      Of_Vertex : Vertex)
     return Edges
   is
      Empty : constant Edges(Positive range 1..0) := (others => <>);

      Vertex_Index : constant Index :=
         Find_Vertex_Data_Index(Inside, Of_Vertex.Id);

      Data : constant Vertex_Data := Inside.Edge_Pointers(Vertex_Index);
   begin
      -- There aren't any outgoing edges
      if (Data.First_Outgoing_Edge = No_Index) then
         return Empty;
      end if;

      declare
         First : constant Maybe_Index := Data.First_Outgoing_Edge;
         Last : constant Maybe_Index :=
            Data.First_Outgoing_Edge + Data.Outgoing_Edges - 1;

         Result : Edges(Positive range 1 .. Data.Outgoing_Edges);
         Result_Index : Positive := 1;
      begin
         for Edge_Index in First .. Last loop
            Result(Result_Index) := Inside.Edges(Edge_Index).Edge;
            Result_Index := Result_Index + 1;
         end loop;

         return Result;
      end;
   end Outgoing_Edges;

   function Incoming_Edges
     (Inside : Graph_Data;
      Of_Vertex : Vertex)
     return Edges
   is
      Empty : constant Edges(Positive range 1..0) := (others => <>);

      Vertex_Index : constant Index :=
         Find_Vertex_Data_Index(Inside, Of_Vertex.Id);

      Vertex_Data : constant Directed_Graph.Vertex_Data :=
         Inside.Edge_Pointers(Vertex_Index);
   begin
      if Vertex_Data.First_Incoming_Edge = No_Index then
         return Empty;
      end if;

      declare
         Result : Edges(Positive range 1 .. Vertex_Data.Incoming_Edges);
         Result_Index : Positive := 1;

         Current_Edge_Index : Maybe_Index := Vertex_Data.First_Incoming_Edge;
         Current_Edge_Data : Edge_Data;
      begin
         while Current_Edge_Index /= No_Index loop
            Current_Edge_Data := Inside.Edges(Current_Edge_Index);
            Result(Result_Index) := Current_Edge_Data.Edge;

            Current_Edge_Index := Current_Edge_Data.Next_Incoming;
            Result_Index := Result_Index + 1;
         end loop;

         return Result;
      end;
   end Incoming_Edges;

   function Neighbours
     (Inside : Graph_Data;
      Of_Vertex : Vertex)
     return Verticies
   is
      Outgoing : constant Edges := Outgoing_Edges(Inside, Of_Vertex);

      Result : Verticies(Outgoing'Range);
      Id : Vertex_Id;
      Vertex_Index : Index;
   begin
      for Index in Result'Range loop
         Id := Outgoing(Index).Source;
         Vertex_Index := Find_Vertex_Data_Index(Inside, Id);
         Result(Index) := Inside.Edge_Pointers(Vertex_Index).Vertex;
      end loop;

      return Result;
   end;

   procedure Dump(Graph : Graph_Data)
   is
      Current_Index : Natural := 0;
   begin
      Put("Last_Created_Vertex: "); Put(Graph.Last_Created_Vertex); New_Line;
      Put("Last_Created_Edge: "); Put(Graph.Last_Created_Edge); New_Line;

      Put_Line("Edge_Pointers:");
      for Value of Graph.Edge_Pointers loop
         Current_Index := Current_Index + 1;
         Put("    ");
         Put(Current_Index);
         Put(": (");
         Put("ID: "); Put(Value.Vertex.Id); Put(", ");
         Put("First_Out: "); Put(Value.First_Outgoing_Edge); Put(", ");
         Put("Num_Out: "); Put(Value.Outgoing_Edges); Put(", ");
         Put("First_In: "); Put(Value.First_Incoming_Edge); Put(", ");
         Put("Num_In: "); Put(Value.Incoming_Edges);
         Put("),");
         New_Line;
      end loop;
      Current_Index := 0;

      Put_Line("Edges:");
      for Value of Graph.Edges loop
         Current_Index := Current_Index + 1;
         Put("    ");
         Put(Current_Index);
         Put(": (");
         Put("ID: "); Put(Value.Edge.Id); Put(", ");
         Put("Source: "); Put(Value.Edge.Source); Put(", ");
         Put("Target: "); Put(Value.Edge.Target); Put(", ");
         Put("Next_In: "); Put(Value.Next_Incoming);
         Put("),");
         New_Line;
      end loop;
   end Dump;
end Directed_Graph;
