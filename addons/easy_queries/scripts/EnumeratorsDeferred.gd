class_name EnumeratorsDeferred

class Enumerable:
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
		
		
	func to_list():
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
		
	# get first in enumerable that satisfies conditions
	func first(cmps):
		return Where.new(self, cmps).at(0)

	func count():
		return to_list().size()
			
	# ===============================================================================
	# Deferred evaluation

	func where(cmps):
		return Where.new(self, cmps)
			
	func project(fields: Array):
		return Project.new(self, fields)
		
	func take(amt):
		return Take.new(self, amt)
		
	func select(select_func: FuncRef, arg=null):
		return Select.new(self, select_func, arg)
		
				
# Pretty much does the same as the native list but is compatible with other enumerators
class ListEnumerator:
	extends Enumerable
	var index
	
	func _init(source: Array).(source):
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
		
	func to_list():
		return [] + source
		
	func at(index):
		if index >= 0 and index < len(source):
			return source[index]
		return null
				
	func reset():
		.reset()
		index = -1

# Get all where preds evaluate to true
class Where:
	extends Enumerable

	var preds
	
	func _init(source, preds).(source):
		self.preds = preds

	func _iter_next(arg):
		if state == RUNNING:
			while source._iter_next(arg):
				var item = source.current
				if Operators.item_valid(item, preds):
					current = item
					return true
			reset()
		return false
		
	func _iter_init(arg):
		._iter_init(arg)
		if not source._iter_init(arg): return false
		
		var item = source.current
		if Operators.item_valid(item, preds):
			current = item
			return true

		return _iter_next(arg)
				
				
# Get data projected into a specified format
class Project:
	extends Enumerable
	
	var fields
	
	func _init(source, fields).(source):
		self.fields = fields
	
	func get_result(item):
		var n = {}
		for f in fields:
			if f in item:
				n[f] = item[f]
		return n
		
	func _iter_next(arg):
		if state == RUNNING:
			if source._iter_next(arg):
				current = get_result(source.current)
				return true

			reset()
		return false
		
	func _iter_init(arg):
		._iter_init(arg)
		if not source._iter_init(arg): return false
		current = get_result(source.current)
		return true
		
# Take first N items from source
class Take:
	extends Enumerable
	
	var count: int
	var i: int
	
	func _init(source, count:int).(source):
		self.source = source
		self.count = count
		self.i=-1
		
	func _iter_next(arg):
		if state == RUNNING:
			if i < count-1 and source._iter_next(arg):
				current = source.current
				i+=1
				return true

			reset()
		return false
		
	func _iter_init(arg):
		._iter_init(arg)
		if not source._iter_init(arg): return false

		i = 0
		current = source.current
		return true
								
	func reset():
		.reset()
		i = -1


# Select from source using a given function reference
class Select:
	extends Enumerable
	
	var select_ref
	var func_arg = null
	
	# expects a func_ref 
	func _init(source, select_ref, arg=null).(source):
		self.select_ref = select_ref
		self.func_arg = arg

	func get_result(item):
		if func_arg == null:
			return select_ref.call_func(item)
		else:
			return select_ref.call_func(item, func_arg)

	func _iter_next(arg):
		if state == RUNNING:
			if source._iter_next(arg):
				current = get_result(source.current)
				return true

			reset()
		return false
		
	func _iter_init(arg):
		._iter_init(arg)
		if not source._iter_init(arg): return false
		current = get_result(source.current)
		return true
		