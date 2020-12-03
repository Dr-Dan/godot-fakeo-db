extends Resource
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")

class Enumerable:
	extends Resource
	const START = 0
	const RUNNING = 1

	var state: int
	var current
	var source

	func _init(source):
		self.source = source
		self.state = START
		self.current = null

	func _iter_next(arg):
		return false

	func _iter_init(arg):
		if state == START:
			state = RUNNING
			return true
		return false

	func reset():
		state = START
		current = null
		
	func _iter_get(arg):
		return current
				
	func to_array():
		var arg = null
		var r = []
		if _iter_init(arg):
			r.append(current)
			while _iter_next(arg):
				r.append(current)
		return r
		
	# ===============================================================================
	# Instant evaluation

	# get item at index in enumerable
	func at(index):
		var i = 0
		if index >= 0 and _iter_init(null):
			if index == 0:
				return current
			while _iter_next(null):
				i+=1
				if i == index:
					return current
		return null
		
# # get first in enumerable that satisfies conditions
	func front(cmps):
		return at(0)

	func back(cmps):
		var result = to_array()
		if result.empty():
			return null
		return result[result.size()-1]
		
	func count():
		return to_array().size()
				
# ===============================================================================

class StepEnumerable:
	extends Enumerable
	
	var index
	
	func source_valid(data):
		return data is Array or data is Enumerable
		
	func set_source(data):
		if data is Enumerable:
			source = data
		elif data is Array:
			source = List.new(data)
		else: return null
		return source

	func _init(source_).(null):
		assert(set_source(source_) != null)
		index = -1

	func _iter_next(arg):
		if state == RUNNING:
			if source._iter_next(arg):
				current = source.current
				index+=1
				return true

			reset()
		return false
		
	func _iter_init(arg):
		._iter_init(arg)
		if not source._iter_init(arg): return false

		index = 0
		current = source.current
		return true
				
	func reset():
		.reset()
		index = -1
		
# Pretty much does the same as the native array but is compatible with other enumerators
class List:
	extends Enumerable
	var index
	
	func _init(source: Array=[]).(source):
		index = -1

	func _iter_next(arg):
		if state == RUNNING:
			if index < source.size()-1:
				index += 1
				current = source[index]
				return true
			reset()
		return false

	func _iter_init(arg):
		._iter_init(arg)
		index = 0
		if index < source.size():
			current = source[index]
			return true
		return false
		
	func to_array():
		return [] + source
		
	func at(index):
		if index >= 0 and index < len(source):
			return source[index]
		return null
				
	func reset():
		.reset()
		index = -1

class Collection:
	extends List
	signal on_item_added(item)
	signal on_item_erased(item)
	signal on_cleared
	
	func _init(source:Array=[]).(source):
		pass
		
	func append(item):
		source.append(item)
		emit_signal("on_item_added", item)
				
	func erase(item):
		if item in source:
			source.erase(item)
			emit_signal("on_item_erased", item)
			
	func erase_at(index):
		assert(index>=0)
		if index < source.size():
			var item = source[index]
			erase(item)
			
	func empty():
		return source.empty()
	
	func clear():
		emit_signal("on_cleared")
		source.clear()
		
	func size():
		return source.size()
	
