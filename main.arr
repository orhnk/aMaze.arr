use context dcic2024

#const_str_dict = [mutable-string-dict: " ", 0, "#", 1, "S", 2, "X", 3]

WALL = "#"
START = "S"
constants  = " #SX"
FIELD_SIZE = 20



maze_map = [array:
  "########",
  "#    X #",
  "# XXXX##",
  "#      #",
  "#  # # #",
  "## ### #",
  "# ##   #",
  "# S  ###",
  "########"
]


WIDTH  = string-length(maze_map.get-now(0))
HEIGHT = array-length(maze_map)
#WARNING: this assumes a rectangular map : )
#this will definitely not bite in the future, especially because I don't implement any tests : >

fun map-block-as-index(char :: String) -> Number:
  if string-length(char) <> 1:
    raise("character length not 1!")
  else:
    if string-index-of(constants, char) == -1:
      raise("invalid character, not in _constants_ string")
    else:
      string-index-of(constants, char)
    end
  end
end

fun set-angle(p, angle :: Number):
  p.angle.set-now(0, angle) 
end
fun set-position-x(p, x :: Number):
  p.position.set-now(0, point-xy(x, p.position.get-now(0).y)) 
end
fun set-position-y(p, y :: Number):
  p.position.set-now(0, point-xy(p.position.get-now(0).x, y)) 
end
fun set-position(p, pos :: Point):
  p.position.set-now(0, pos) 
end

fun get-angle(p) -> Number:
  p.angle.get-now(0)
end
fun get-position-x(p) -> Number:
  p.position.get-now(0).x
end
fun get-position-y(p) -> Number:
  p.position.get-now(0).y 
end
fun get-position(p) -> Point:
  p.position.get-now(0) 
end



fun fill-game(arr, map_arr, Width:: Number, Height:: Number) -> Point:
  block:
  var ret = point-xy(0,0)
  for each(i from range(0, Height)):
    for each(j from range(0, Width)):
      block:
      strg  = map_arr.get-now(i)
      char = string-char-at(strg, j)
          when string-equal(START, char):
        ret := point-xy(j, i) 
      end
      
      arr.get-now(i).set-now(j, map-block-as-index(char))
      end
    end
  end
  ret
end
end

fun init-game-array(arr, Width:: Number, Height:: Number):
  for each(h from range(0, WIDTH)):
    arr.set-now(h, array-of(0, WIDTH))
  end
end


#var player =  make_player( point-xy(0,0), 0, 90)

var player = {
  position: [array: point-xy(0,0)],
  angle:    [array: 0],
  fov: 90
}

game_field = array-of(array-of(0, WIDTH), (HEIGHT))
init-game-array(game_field, WIDTH, HEIGHT)
start_pos = fill-game(game_field, maze_map, WIDTH, HEIGHT)

fun num-square(n :: Number) -> Number:
  n * n
end

fun get-distance(start_ :: Point, end_ :: Point) -> Number:
  num-sqrt(num-square(start_.x - end_.x) + num-square(start_.y - end_.y))
end


fun step_rays(angle :: Number, position :: Point) -> Array:
  ret = game_field.get-now(position.y).get-now(position.x)
  if ret <> map-block-as-index(WALL):
    increment-y = num-round(num-cos(angle * PI))
    increment-x = num-round(num-sin(angle * PI))
    new_pos = point-xy(position.x + increment-x, position.y - increment-y) #y points downward
    
    step_rays(angle, new_pos)
  else:
    [array: ret, point-xy(position.x, position.y)]
  end
end


#fun draw_maze(p, maze:: Array) -> Image:
  
#end