with Ada.Containers.Vectors;

package Vector_Utils is
   generic
      with package Vect is new
        Ada.Containers.Vectors (others => <>);
   function Contains_Any_Matching
     (Container : Vect.Vector;
      Predicate : not null access function
        (element : Vect.Element_Type)
        return Boolean)
     return Boolean;
   
   type Search_Hint is (Smaller, Bigger, Correct);
   generic
      with package Vect is new
        Ada.Containers.Vectors (others => <>);
   function Find_Matching
     (Container : Vect.Vector;
      Judge : not null access function (Element : Vect.Element_Type) return Search_Hint;
      Index_Hint : Vect.Extended_Index := Vect.No_Index;
      Upper_Bound_Hint : Vect.Extended_Index := Vect.No_Index;
      Lower_Bound_Hint : Vect.Extended_Index := Vect.No_Index)
     return Vect.Extended_Index;

end Vector_Utils;
