use context dcic2024
include reactors


WALL = "#"
START = "S"
constants  = " #SX"
FIELD_SIZE = 20

SCREEN_WIDTH = 900
SCREEN_HEIGHT = 600


DIR_UP    = 0
DIR_RIGHT = 90
DIR_DOWN  = 180
DIR_LEFT  = 270

RAY_COUNT = 32
RECT_HEIGHT = SCREEN_HEIGHT * 0.7


debug_colors = [array: "gray", "blue", "orange", "red"]

maze_map = [array:
  "########",
  "#    X #",
  "# ######",
  "# ######",
  "#     S#",
  "# ######",
  "#   ## #",
  "#      #",
  "########"
]


WIDTH  = string-length(maze_map.get-now(0))
HEIGHT = array-length(maze_map)
#WARNING: this assumes a rectangular map : )
#this will definitely not bite in the future, especially because I don't implement any tests : >
fun num-degrees-radians(degrees :: Number) -> Number:
  (degrees ) * (1 / 180)
  #Usage: 
  #UP:= 0
  #RIGHT:=90
  #DOWN:=180
  #LEFT:=270
end
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
  if (pos.x < (WIDTH - 1)) and (pos.x >= 0) and (pos.y >= 0) and (pos.y < (HEIGHT - 1)):
    
    p.position.set-now(0, pos) 
  else:
    nothing
  end
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



fun increment-position-x(p, inc :: Number, game_field):
  x = get-position-x(p)
  if ((x + inc) <= (HEIGHT - 1)) and ((x + inc) >= 0):
    rn_x = num-round(x + inc)
    rn_y = num-round(get-position-y(p))
    char_ = game_field.get-now(rn_y).get-now(rn_x)
    if char_ <> map-block-as-index("#"):
      set-position-x(p, x + inc)
    else:
      nothing
    end
  else:
    nothing
  end
end

fun increment-position-y(p, inc :: Number, game_field):
  y = get-position-y(p)
  if ((y + inc) <= (HEIGHT - 1)) and ((y + inc) >= 0):
    rn_y = num-round(y + inc)
    rn_x = num-round(get-position-x(p))
    char_ = game_field.get-now(rn_y).get-now(rn_x)
    if char_ <> map-block-as-index("#"):
        set-position-y(p, y + inc)
    else:
      nothing
    end
  else:
    nothing
  end 
end

fun angled_increment-position(p, inc :: Number, gamefield):
  block:
    increment-y = inc * (num-cos(get-angle(p) * PI))
    increment-x = inc * (num-sin(get-angle(p) * PI))
    increment-position-x(p, increment-x, gamefield)
    increment-position-y(p, increment-y, gamefield)
  end
end
fun increment-angle(p, inc :: Number):
  angle = get-angle(p)
  set-angle(p, angle + inc)
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
  for each(h from range(0, Height)):
    arr.set-now(h, array-of(0, Width))
  end
end



var player = {
  position: [array: point-xy(5,3)],
  angle:    [array: DIR_RIGHT],
  fov: 90
}




game_field = array-of(array-of(0, WIDTH), (HEIGHT))
init-game-array(game_field, WIDTH, HEIGHT)
start_pos = fill-game(game_field, maze_map, WIDTH, HEIGHT)
set-position(player, start_pos)

fun num-square(n :: Number) -> Number:
  n * n
end

fun get-distance(start_ :: Point, end_ :: Point) -> Number:
  num-sqrt(num-square(start_.x - end_.x) + num-square(start_.y - end_.y))
end


fun step_rays(angle :: Number, position :: Point) -> Array:
  block:
  var x = num-round(position.x)
  var y = num-round(position.y)
  
    if x <= 0:
    x := 0
    else if x >= (WIDTH - 1):
    x := (WIDTH  - 1)
    else: nothing
  end
    if y <= 0:
    y := 0
    else if y >= (HEIGHT  - 1):
    y := (HEIGHT  - 1)
    else: nothing
  end
  
  ret = game_field.get-now(y).get-now(x)
  if ret <> map-block-as-index(WALL):
      increment-y = (num-cos(angle * PI)) * 0.01
      increment-x = (num-sin(angle * PI)) * 0.01
      new_pos = point-xy(position.x + increment-x, position.y + increment-y) 
    
    step_rays(angle, new_pos)
  else:
    [array: ret, point-xy(position.x, position.y)]
  end
  end
end





fun draw-wall(distance, color :: String) -> Image:
  rectangle(SCREEN_WIDTH / RAY_COUNT,  RECT_HEIGHT * (1 / num-max(distance + 1, 1)), "solid" , color)
  
  


end
  
#for loops are for noobs

fun iter(counter :: Number, max :: Number, return_, player_) -> Image:
  
  if counter >= max:
    return_
  else:
    block:
      rounded-pos = point-xy(num-round(get-position-x(player_)), num-round(get-position-y(player_)))
      deg = (get-angle(player_) + num-degrees-radians((player_.fov * -0.5) + counter))
      res = step_rays(deg, rounded-pos).get-now(1)
      scale_ = 1 #+ (num-abs(counter - (max * 0.5)) / (max * 100))
      distance = (get-distance(get-position(player_), res)) * scale_
      var ret = if distance > 5:
        draw-wall(distance, "gray")
      else if distance > 2.5:
        draw-wall(distance, "blue")
        else if distance > 1:
        draw-wall(distance, "red")
      else:
        draw-wall(distance, "green")
      end
      beside(ret, iter(counter + 1, max, ret, player))
      
    end
  end
end


#TODO: handle input properly (wrap around array bounds)
fun move-forward(x, key):
  if string-equal(key, "up"):
    block:
      angled_increment-position(player, 0.05, game_field)
      x
    end
  else if string-equal(key, "down"):
    block:
      angled_increment-position(player, -0.05, game_field)
      x
    end
  else if string-equal(key, "left"):
    block:
      increment-angle(player, -0.005)
      x
    end
  else if string-equal(key, "right"):
    block:
      increment-angle(player, 0.005)
      x
    end
  else:
    
    x
  end
end

fun render(x):
  var target = rectangle(SCREEN_WIDTH, SCREEN_HEIGHT, "solid", "black")
  
  place-image(iter(0, RAY_COUNT, target, player),SCREEN_WIDTH * 0.5,SCREEN_HEIGHT * 0.5,target)
end
fun increment(x): x + 1 end

#TODO: add DT and make the array accesses check and clamp bounds, make the position floating point but round it on array access for the map (movement almost works)

main = reactor:
  seconds-per-tick: 0.0006,
  title: "raycaster",
  on-tick: increment,
    init: 0,
  on-key: move-forward,
  to-draw: render
end


 interact(main)