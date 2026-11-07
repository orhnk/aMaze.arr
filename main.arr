use context dcic2024
include tables

data Player:
    make_player(position :: Point, angle :: Number, fov :: Number)
end
constants  = " #SX"
FIELD_SIZE = 20


#const_str_dict = [mutable-string-dict: " ", 0, "#", 1, "S", 2, "X", 3]



maze_map = [array:
  "########",
  "#    X #",
  "# ######",
  "#      #",
  "#  # # #",
  "## ### #",
  "# ##   #",
  "#S   ###",
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


#map_arr = for map-block-as-index(string-char-at( "#SX##", element from range(0,3))): 
#  element+1
#end



fun fill-game(arr, map_arr, Width:: Number, Height:: Number) -> Point:
  block:
  var ret = point-xy(0,0)
  for each(i from range(0, Height)):
    for each(j from range(0, Width)):
      block:
      strg  = map_arr.get-now(i)
      char = string-char-at(strg, j)
      when string-equal("S", char):
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


game_field = array-of(array-of(0, WIDTH), (HEIGHT))
init-game-array(game_field, WIDTH, HEIGHT)
start_pos = fill-game(game_field, maze_map, WIDTH, HEIGHT)


var player = make_player(start_pos, 0, 90)
