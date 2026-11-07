use context dcic2024
include tables
data Player:
    player(x:: Number, y:: Number, angle:: Number, fov:: Number)
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



fun fill-game-array(arr, map_arr, Width:: Number, Height:: Number):

  for each(i from range(0, Height)):
    for each(j from range(0, Width)):
      strg  = map_arr.get-now(i)
      char = string-char-at(strg, j)
      arr.get-now(i).set-now(j, map-block-as-index(char))
    end
  
  end
end
fun init-game-array(arr, Width:: Number, Height:: Number):
  for each(h from range(0, WIDTH)):
    arr.set-now(h, array-of(0, WIDTH))
  end
end

game_field = array-of(array-of(0, WIDTH), (HEIGHT))
init-game-array(game_field, WIDTH, HEIGHT)
fill-game-array(game_field, maze_map, WIDTH, HEIGHT)

