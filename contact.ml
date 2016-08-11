module Contact =
struct

type field =
  All | Id | Name | Surname | Age | Email | Phone;;

type contact =
  {
    surname : string;
    name : string;
    age : int;
    mail : string;
    phone : string;
  };;
  
let create_node (s, n, a, m, p) =
  {
    surname = String.capitalize(String.lowercase s);
    name = String.uppercase(n);
    age = a;
    mail = m;
    phone = p;
  };;

let rec elem_empty = function
  | [] -> false
  | str::l when (String.length(str) <= 0) -> true
  | str::l -> elem_empty l;;

let verif = function
  | (s, n, a, m, p) when elem_empty(s::n::m::p::[])-> false
  | (s, n, a, m, p) when a < 0 || a > 120 -> false
  | (s, n, a, m, p) when (String.length(p) != 10) -> false
  | (s, n, a, m, p) when (String.contains m '@') = false -> false
  | (s, n, a, m, p) when (String.contains_from m (String.index m '@') '.') = false -> false
  | (s, n, a, m, p) when (String.index m '.') = (String.length(m)) - 1 -> false
  | (s, n, a, m, p) when (String.index m '@') = 0 -> false
  | (s, n, a, m, p) when (String.index m '.') - (String.index m '@') <= 1 -> false
  | _ -> true;;


let add list node =
  let rec rev_rec l1 l2 = match l1 with
    | [] -> l2
    | a::b -> rev_rec b (a::l2)
  in
  let rec add_rec l1 l2 node = match l1 with
    | [] -> (create_node node)::l2
    | a::b -> add_rec b (a::l2) node
  in
  if (verif node) then 
    rev_rec (add_rec list [] node) []
  else
    raise (Invalid_argument "Contact.Add_Contact_With_Invalid_Data.");;

let getId list field str =
  let search_by_id l str =
    let b = try ignore (int_of_string(str)); int_of_string(str) with _ -> -1 
    in
    let rec search_rec l c = match l with
      | [] when c > 0 -> -1
      | [] -> b
      | e::l -> search_rec l (c - 1)
    in
    search_rec l b
  in
let search_by_all l str =
  let rec search_rec l str id = match l with
    | [] -> -1
    | a::b when String.lowercase a.surname = String.lowercase str
	   || String.lowercase a.name = String.lowercase str
	     || String.lowercase a.mail = String.lowercase str
	       || String.lowercase a.phone = String.lowercase str
		 || try ignore (int_of_string str); true
		   && a.age = (int_of_string str) with _ -> false -> id
		     
		   | a::b -> search_rec b str (id - 1)
  in
  search_rec l str ((List.length l) - 1)
in
let search_by_name l str =
  let rec search_rec l str id = match l with
    | [] -> -1
    | a::b when String.lowercase a.name = String.lowercase str -> id
    | a::b -> search_rec b str (id - 1)
  in
  search_rec l str ((List.length l) - 1)
in
let search_by_surname l str =
  let rec search_rec l str id = match l with
    | [] -> -1
    | a::b when String.lowercase a.surname = String.lowercase str -> id
    | a::b -> search_rec b str (id - 1)
  in
  search_rec l str ((List.length l) - 1)
in
let search_by_age l str =
  let rec search_rec l str id = match l with
    | [] -> -1
    | a::b when try ignore (int_of_string str); true 
      && a.age = (int_of_string str) with _ -> false -> id
      | a::b -> search_rec b str (id - 1)
  in
  search_rec l str ((List.length l) - 1)
in
let search_by_email l str =
  let rec search_rec l str id = match l with
    | [] -> -1
    | a::b when String.lowercase a.mail = String.lowercase str -> id
    | a::b -> search_rec b str (id - 1)
  in
  search_rec l str ((List.length l) - 1)
in
let search_by_phone l str =
  let rec search_rec l str id = match l with
    | [] -> -1
    | a::b when String.lowercase a.phone = String.lowercase str -> id
    | a::b -> search_rec b str (id - 1)
  in
  search_rec l str ((List.length l) - 1)
in
match field with
| f when f = All -> search_by_all (List.rev list) str
| f when f = Id -> search_by_id (List.rev list) str
| f when f = Name -> search_by_name (List.rev list) str
| f when f = Surname -> search_by_surname (List.rev list) str
| f when f = Age -> search_by_age (List.rev list) str
| f when f = Email -> search_by_email (List.rev list) str
| f when f = Phone -> search_by_phone (List.rev list) str
| _ -> search_by_all list str;;
    
let remove list id =
  let rec remove_rec l1 l2 id = match l1 with
    | [] -> raise (Invalid_argument "Remove_Using_An_Invalid_Id.");
    | a::b when id = 0 -> (List.rev l2)@b
    | a::b -> remove_rec b (a::l2) (id - 1)
  in
  if (list = [])
  then begin raise (Invalid_argument "Remove_Using_An_Invalid_Id.");end
  else begin 
      if (id < 0)
      then begin raise (Invalid_argument "Remove_Using_An_Invalid_Id.");end
  else begin 
    remove_rec list [] id end end;;

let replace list id node =
  let rec replace_rec l1 l2 id n = match list with
    | [] -> raise (Invalid_argument "Replace_Impossible_On_An_Empty_List.");
    | a::b when id < 0 -> raise (Invalid_argument "Replace_Using_An_Invalid_Id.");
    | a::b when id = 0 -> (List.rev l2)@((create_node n)::b)
    | a::b -> replace_rec b (a::l2) (id - 1) n
  in
  if (verif node) then 
    replace_rec list [] id node
  else
    raise (Invalid_argument "Replace_Using_An_Invalid_Id.");;

let rec getNode list id = match list with
  | [] -> create_node ("", "", -1, "", "")
  | a::b when id < 0 -> raise (Invalid_argument "Replace_Using_An_Invalid_Id.");
  | a::b when id = 0 -> a
  | a::b -> getNode b (id - 1);;

let disp_str str pad =
  let rec disp_space nb =
    if nb > 0 then begin  
      print_string " ";
      disp_space (nb - 1) end
  in
  let len = String.length(str)
  in
  print_string str;
  disp_space (len - pad);;

let rec print list field str =
  let disp_node node id =
    disp_str (string_of_int id) 4;
    disp_str node.surname 16;
    disp_str node.name 16;
    disp_str (string_of_int node.age) 4;
    disp_str node.mail 32;
    disp_str node.phone 14;
    print_newline ();
  in
  let id = getId list field str
  in
if id >= 0 then begin  
  disp_node (getNode list (id)) id;
  print (remove list id) field str end;;

end;;

open Contact;;

let l = [];;

let l = Contact.add [] ("axEl", "Drozdz", 21, "axel@epitech.eu", "0606060660");;
let l = Contact.add l ("niColas", "Laurent", 23, "nicolas@epitech.eu", "0606060660");;
let l = Contact.add l ("elIse", "maRtin", 23, "elise@epitech.eu", "0606060660");;

let l = Contact.replace l 0 ("emMa", "Dubois", 23, "emma@epitech.eu", "0606060680");;

let l = Contact.remove l 1;;

Contact.print l Contact.All "23";;
