package body Vector_Utils is
   function Contains_Any_Matching
     (Container : Vect.Vector;
      Predicate : not null access function
        (element : Vect.Element_Type)
        return Boolean)
     return Boolean is
      (for some Value of Container =>
         Predicate(Value));

   function Find_Matching
     (Container : Vect.Vector;
      Index_Hint : Vect.Extended_Index := Vect.No_Index;
      Upper_Bound_Hint : Vect.Extended_Index := Vect.No_Index;
      Lower_Bound_Hint : Vect.Extended_Index := Vect.No_Index)
   return Vect.Extended_Index
   is
      use Ada.Containers;
      use Vect;

      High_Hint : constant Index_Type :=
        (if Upper_Bound_Hint = Vect.No_Index
           then Container.Last_Index
           else Index_Hint);

      Low_Hint : constant Index_Type :=
        (if Lower_Bound_Hint = Vect.No_Index
           then Container.First_Index
           else Index_Hint);

      Mid_Hint : constant Index_Type :=
        (if Index_Hint = Vect.No_Index
           then (Low_Hint + High_Hint) / 2
           else Index_Hint);

       Low  : Index_Type := Low_Hint;
       High : Index_Type := High_Hint;
       Mid  : Index_Type := Mid_Hint;
   begin
      -- TODO: Take proximity to hint into account
      -- and linear search for a bit before trying

      if Container.Length = 0 then
         return No_Index;
      end if;

      loop
         case Judge(Container(Mid)) is
            when Bigger =>
               exit when Low = Mid;
               High := Mid - 1;
            when Smaller =>
               exit when High = Mid;
               Low := Mid + 1;
            when Correct =>
               return Mid;
         end case;
         Mid := (Low + High) / 2;
      end loop;

      return No_Index;
   end Find_Matching;

end Vector_Utils;
