module Contact :
sig
  type field =
      All | Id | Name | Surname | Age | Email | Phone
  
  type contact =
    {
      surname : string;
      name : string;
      age : int;
      mail : string;
      phone : string;
    }

  val add : contact list -> string * string * int * string * string -> contact list
  val getId : contact list -> field -> string -> int  
  val remove : contact list -> int -> contact list
  val replace : contact list -> int -> string * string * int * string * string -> contact list   
  val print : contact list -> field -> string -> unit
end;;
