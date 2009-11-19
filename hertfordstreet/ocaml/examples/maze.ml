(* The board size and line width used to render it. *)
let line_width = 8

(* Test if a position has not yet been visited. *)
let unvisited v (x, y) = try v.(y).(x) with _ -> false

(* Mark a position as having been visited. *)
let visit v (x, y) = try v.(y).(x) <- false with _ -> ()

let choices visited (x, y) =
  List.filter (unvisited visited) [x-1, y; x+1, y; x, y-1; x, y+1]

(* Randomly pick an unvisited neighbor. *)
let random_neighbor visited cell = match choices visited cell with
    [] -> None
  | choices -> Some (List.nth choices (Random.int (List.length choices)))

(* Draw to the given coordinate. *)
let vertex (x, y) =
  GlDraw.vertex2 (float line_width *. (0.5 +. float x),
                  float line_width *. (0.5 +. float y))

(* Recurse over the maze. *)
let rec draw_maze_aux n visited cell k =
  vertex cell;
  GlDraw.ends ();
  GlDraw.begins `points;
  vertex cell;
  GlDraw.ends ();
  GlDraw.begins `line_strip;
  vertex cell;
  visit visited cell;
  match random_neighbor visited cell with
    Some neighbor ->
      draw_maze_aux n visited neighbor
        (fun () -> draw_maze_aux n visited cell k)
  | _ ->
      GlDraw.ends ();
      GlDraw.begins `line_strip;
      k()

(* Render a maze. *)
let draw_maze n =
  GlDraw.begins `line_strip;
  vertex (0, 0);
  draw_maze_aux n (Array.make_matrix n n true) (0, 0) (fun _ -> ());
  GlDraw.ends ();
  flush stdout

let n = ref 1 and width = ref 1 and height = ref 1

(* Setup a 2D orthogonal projection. *)
let set_projection () =
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.ortho ~x:(0., float !width) ~y:(0., float !height) ~z:(0., 1.);
  GlMat.mode `modelview;
  GlMat.load_identity ()

(* Resize the window, generating a new maze. *)
let reshape ~w ~h =
  GlDraw.viewport 0 0 w h;
  n := (min w h) / line_width;
  width := w; height := h;
  set_projection ();
  Glut.postRedisplay ()

(* Memoize a function in a display list. *)
let memoize k =
  let d = Hashtbl.create 1 in
  fun x ->
    try GlList.call (Hashtbl.find d x) with Not_found ->
      let list = GlList.create `compile in
      k x;
      GlList.ends ();
      GlList.call list;
      Hashtbl.add d x list

(* Setup glut and enter the glut mainloop. *)
let () =
  ignore (Glut.init Sys.argv);
  ignore (Glut.createWindow "Maze");
  GlDraw.line_width (float (line_width - 2));
  GlDraw.point_size (float (line_width - 2));
  Glut.reshapeFunc ~cb:reshape;
  let draw = memoize draw_maze in
  Glut.displayFunc (fun () -> GlClear.clear [`color]; draw !n; Gl.flush ());
  Glut.keyboardFunc ~cb:(fun ~key ~x ~y -> if key=27 then exit 0);
  Glut.mainLoop ()
