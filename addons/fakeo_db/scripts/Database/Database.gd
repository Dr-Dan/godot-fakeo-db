const Collection = preload("res://addons/fakeo_db/scripts/Database/Collection.gd")
var collections = {}

func add_collection(c:Collection, name):
	assert(not name in collections)
	collections[name] = c

func get_collection(name) -> Collection:
	if name in collections:
		return collections[name]
		
	collections[name] = Collection.new()
	return collections[name]