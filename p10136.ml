(* problem: https://uva.onlinejudge.org/external/101/10136.pdf *)

open Core.Std;;


(* geometry/vector utilities  *)

let law_of_cosines : float -> float -> float -> float =
  fun a b c -> acos ((a ** 2. +. b ** 2. -. c ** 2.) /.
                     (2. *. a *. b))
;;

let rotate : float -> (float * float) -> (float * float) =
  fun t (x, y) ->
  (
    cos(t) *. x -. sin(t) *. y,
    sin(t) *. x +. cos(t) *. y
  )
;;

let unitize : (float * float) -> (float * float) =
  fun (x, y) ->
  let l = sqrt (x ** 2. +. y ** 2.) in
  (x /. l, y /. l)
;;

let times : float -> (float * float) -> (float * float) =
  fun s (x, y) -> (s *. x, s *. y)
;;

let plus : (float * float) -> (float * float) -> (float * float) =
  fun (x, y) (x', y') -> (x +. x', y +. y')
;;


(* constants *)

let plane_edge_length = 50.0;;
let cookie_diameter = 5.0;;


(* algorithm *)

let pairs : 'a list -> ('a * 'a) list = (* ordered pairs *)
  fun ls ->
  ls
  |> List.map ~f:(fun x ->
                  ls
                  |> List.filter ~f:(fun y -> not (x = y))
                  |> List.map ~f:(fun y -> (x, y))
                 )
  |> List.concat
;;

let cookie_center : (float * float) -> (float * float) -> (float * float) =
  fun (x0, y0) (x1, y1) ->
  let (vx, vy) = (x1 -. x0, y1 -. y0) in
  let vl = sqrt (vx ** 2. +. vy ** 2.) in
  let theta = law_of_cosines cookie_diameter vl cookie_diameter in
  rotate theta (vx, vy)
  |> unitize
  |> times cookie_diameter
  |> plus (x0, y0)
;;

let in_cookie : (float * float) -> (float * float) -> bool =
  fun (cookie_x, cookie_y) (x, y) ->
  (x -. cookie_x) ** 2. +. (y -. cookie_y) ** 2. <= cookie_diameter ** 2.
;;

let solve : (float * float) list -> int =
  fun points ->
  pairs points
  |> List.map ~f:(fun (p0, p1) -> cookie_center p0 p1)
  |> List.map ~f:(fun center ->
                  points
                  |> List.filter ~f:(in_cookie center)
                  |> List.length
                 )
  |> List.fold ~init:0 ~f:max
;;


(* spec *)

let pi = 2. *. acos 0.;;
let (==.) x y = x <=. y && y <=. x;;

let () =
  [
    (
      let (x, y) = rotate (pi /. 2.) (2., 1.) in
      x ==. -1. && y ==. 2.
    );
    (
      let t = (law_of_cosines 1. 5. 5.) /. pi in
      t ==. 0.468115719571
    );
    pairs [1; 2; 3] = [(1, 2); (1, 3); (2, 1); (2, 3); (3, 1); (3, 2)];
    (
      let (x, y) = cookie_center (4., 4.) (4., 5.) in
      x ==. -0.974937185533 && y ==. 4.5
    );
    solve [
      (4., 4.);
      (4., 5.);
      (5., 6.);
      (1., 20.);
      (1., 21.);
      (1., 22.);
      (1., 25.);
      (1., 26.)
    ] = 4
  ]
  |> List.map ~f:(printf "%B\n")
  |> fun _ -> flush_all ()
;;
