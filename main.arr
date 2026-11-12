
include reactors
import color as C


WALL = "#"
START = "S"
constants  = " #SX"
FIELD_SIZE = 20
FOV = 90

SCREEN_WIDTH = 900
SCREEN_HEIGHT = 600

PLAYER_SPEED = 3
TURN_SPEED = 0.3


DIR_UP    = 0
DIR_RIGHT = 90
DIR_DOWN  = 180
DIR_LEFT  = 270

RAY_COUNT = 90
RECT_HEIGHT = SCREEN_HEIGHT * 0.7


maze_map = [array:
  "#################",
  "#    X          #",
  "# #    S      # #",
  "##         ###  #",
  "#               #",
  "#  #   #     ## #",
  "# #    #   ##   #",
  "##     #        #",
  "#################"
]


WIDTH  = string-length(maze_map.get-now(0))
HEIGHT = array-length(maze_map)
#WARNING: this assumes a rectangular map : )
#this will definitely not bite in the future, especially because I don't implement any tests : >

# data Direction:
#   | forward
#   | backward
#   | right
#   | left
# end

# data Action:
#   | walk(d :: Direction)
#   | shoot
#   | interact
# end

fun num-degrees-radians(d :: Number) -> Number:
  (d) * (1 / 180)
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

fun fill-game(calculated_map_buf, map_arr, Width:: Number, Height:: Number) -> Point:
  block:
    var ret = point-xy(0,0)
    for each(i from range(0, Height)):
      for each(j from range(0, Width)):
        block:
          maze_map_row  = map_arr.get-now(i)
          maze_map_block = string-char-at(maze_map_row, j)
          when string-equal(START, maze_map_block):
            ret := point-xy(j, i) 
          end
          calculated_map_buf.get-now(i).set-now(j, map-block-as-index(maze_map_block))
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


data Player:
  | player(
      ref position :: Array<Point>,
      ref angle :: Array<Number>,
      ref fov :: Number
  )
sharing:
  method set-angle(self, angle :: Number):
    self!angle!set-now(0, angle) 
  end,
  method set-position-x(self, x :: Number):
    self!position!set-now(0, point-xy(x, self!position.get-now(0).y)) 
  end,
  method set-position-y(self, y :: Number):
    self!position!set-now(0, point-xy(self!position.get-now(0).x, y)) 
  end,
  method set-position(self, pos :: Point):
    if (pos.x < (WIDTH - 1)) and (pos.x >= 0) and (pos.y >= 0) and (pos.y < (HEIGHT - 1)):
    
      self!position.set-now(0, pos) 
    else:
      nothing
    end
  end,

  method get-angle(self) -> Number:
    self!angle.get-now(0)
  end,
  method get-position-x(self) -> Number:
    self!position.get-now(0).x
  end,
  method get-position-y(self) -> Number:
    self!position.get-now(0).y 
  end,
  method get-position(self) -> Point:
    self!position.get-now(0) 
  end,



  method increment-position-x(self, inc :: Number):
    x = self.get-position-x()
    if ((x + inc) <= (WIDTH - 1)) and ((x + inc) >= 0):
      self.set-position-x(x + inc)
    else:
      nothing
    end
  end,

  method increment-position-y(self, inc :: Number):
    y = self.get-position-y()
    if ((y + inc) <= (HEIGHT - 1)) and ((y + inc) >= 0):
      self.set-position-y(y + inc)
    else:
      nothing
    end
  end,

  method angled_increment-position(self, inc :: Number):
    block:
      increment-y = inc * (num-cos(self.get-angle() * PI))
      increment-x = inc * (num-sin(self.get-angle() * PI))
      self.increment-position-x(increment-x)
      self.increment-position-y(increment-y)
    end
  end,
  method increment-angle(self, inc :: Number):
    angle = self.get-angle()
    self.set-angle(angle + inc)
  end
end

var p = player(
  [array: point-xy(5,3)],
  [array: DIR_RIGHT],
  FOV
)

game_field = array-of(array-of(0, WIDTH), (HEIGHT))
init-game-array(game_field, WIDTH, HEIGHT)
start_pos = fill-game(game_field, maze_map, WIDTH, HEIGHT)
p.set-position(start_pos)

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
  
    if x < 0:
    x := 0
  else if x > (WIDTH  - 1):
    x := (WIDTH  - 1)
    else: nothing
  end
  if y < 0:
    y := 0
  else if y > (HEIGHT  - 1):
    y := (HEIGHT  - 1)
    else: nothing
  end
  
  ret = game_field.get-now(y).get-now(x)
  if ret <> map-block-as-index(WALL):
    increment-y = (num-cos(angle * PI))
    increment-x = (num-sin(angle * PI))
      new_pos = point-xy(position.x + increment-x, position.y + increment-y) 
    
    step_rays(angle, new_pos)
  else:
    [array: ret, point-xy(position.x, position.y)]
  end
  end
end





fun draw-wall(distance, wall_color :: C.Color) -> Image:
  rectangle(SCREEN_WIDTH / RAY_COUNT,  RECT_HEIGHT * (1 / num-max(distance + 1, 1)), "solid" , wall_color)
end
  
#for loops are for noobs

fun iter(counter :: Number, max :: Number, return_, p_) -> Image:
  
  if counter >= max:
    return_
  else:
    block:
      rounded-pos = point-xy(num-round(p_.get-position-x()), num-round(p_.get-position-y()))
      deg = (p_.get-angle() + num-degrees-radians((-45) + counter))
      res = step_rays(deg, rounded-pos).get-now(1)
      scale_ = 1 #+ (num-abs(counter - (max * 0.5)) / (max * 100))
      distance = (get-distance(p_.get-position(), res)) * scale_
      base_color = 255 - (distance * 11)
      var ret = draw-wall(distance, C.color(
        255,
        base_color,
        base_color,
        1
      ))
      beside(ret, iter(counter + 1, max, ret, p))
    end
  end
end


#TODO: handle input properly (wrap around array bounds)
fun move-forward(x, key):
  if string-equal(key, "up"):
    block:
      p.angled_increment-position(0.02 * PLAYER_SPEED)
      x
    end
  else if string-equal(key, "down"):
    block:
      p.angled_increment-position(-0.02 * PLAYER_SPEED)
      x
    end
  else if string-equal(key, "left"):
    block:
      p.increment-angle(-0.05 * TURN_SPEED)
      x
    end
  else if string-equal(key, "right"):
    block:
      p.increment-angle(0.05 * TURN_SPEED)
      x
    end
  else:
    x
  end
end

fun render(x):
  var target = rectangle(SCREEN_WIDTH, SCREEN_HEIGHT, "solid", "black")
  
  place-image(iter(0, RAY_COUNT, target, p), SCREEN_WIDTH * 0.5 ,SCREEN_HEIGHT * 0.5 ,target)
end
fun increment(x): x + 1 end

#TODO: add DT and make the array accesses check and clamp bounds, make the position floating point but round it on array access for the map (movement almost works)

main = reactor:
  # seconds-per-tick: 0.016,
  title: "Count by 10",
  on-tick: increment,
  init: 0,
  on-key: move-forward,
  to-draw: render
end


interact(main)
