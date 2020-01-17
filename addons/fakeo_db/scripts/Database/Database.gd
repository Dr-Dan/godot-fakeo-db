const Collection = preload("res://addons/fakeo_db/scripts/Enumerables.gd").Collection

var collections = {}

func add_collection(c:Collection, name):
	assert(not name in collections)
	collections[name] = c

func has_collection(name):
	pass
	
func get_collection(name) -> Collection:
	if name in collections:
		return collections[name]
		
	collections[name] = Collection.new()
	return collections[name]

func erase(name):
	if name in collections:
		collections.erase(name)