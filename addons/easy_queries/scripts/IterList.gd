var start
var curr
var end
var increment
var items = []

func _init(items=[]):
	increment = 1
	start = 0
	curr = start
	self.items = items
	self.end = items.size()
	
# TODO: iterator shouldn't have size
func size():
	return items.size()

func empty():
	return size() == 0
	
func append(item):
	# if not item in items:
	items.append(item)

func append_many(items):
	for i in items:
		append(i)
		
func erase(item):
	if item in items:
		items.erase(item)
		
func erase_many(items):
	for i in items:
		erase(i)
		
func at(i):
	return items[i]
	
# NOTE: this may be overridden for child classes
func should_continue():
	return (curr < end)

func _iter_init(arg):
	curr = start
	return should_continue()

func _iter_next(arg):
	curr += increment
	return should_continue()

# TODO: return next evaluated item
func _iter_get(arg):
	return items[curr]
	

func get_fields(fields: Array):
	var result = []
	for item in items:
		var n = {}
		for f in fields:
			if f in item:
				n[f] = item[f]
		if not n.empty():
			result.append(n)
	return get_script().new(result)
	
func get_values(field):
	var result = []
	for item in items:
		if field in item:
			result.append(item[field])
#			if not n.empty():
#				result.append(n)
	return result
#	return get_script().new(result)
	
func has_key(field):
	for item in items:
		if not field in item:
			return true
	return false
	

func where(cmps):
	var result = []
	for item in items:
		for key in cmps:
			if key in item:
				if cmps[key].eval(item[key]):
					result.append(item)
	return get_script().new(result)
	
func count(cmps) -> int:
	var result = 0
	for item in items:
		for key in cmps:
			if key in item and cmps[key].eval(item[key]):
					result += 1
	return result
		
func select(fields: Array):
	var result = []
	for item in items:
		var n = {}
		for f in fields:
			n[f] = item[f]
		if not n.empty():
			result.append(n)
	return get_script().new(result)
	

func first(cmps):
	if items.empty(): return null
	for item in items:
		for key in cmps:
			if key in item and cmps[key].eval(item[key]):
					return item
	return null

func take(cmps, amt):
	var result = []
	for item in items:
		for key in cmps:
			if key in item and cmps[key].eval(item[key]):
					result.append(item)
					if result.size() >= amt:
						return get_script().new(result)

	return get_script().new(result)
		
func to_list(deep=false):
	return items.duplicate(deep)
