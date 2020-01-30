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
		
	# get first in enumerable that satisfies conditions
	func first(cmps):
		return where(cmps).at(0)
		
	# returns true if any match conditions
	func any(cmps):
		var d = first(cmps)
		return d != null

	func count():
		return to_array().size()
			
	# ===============================================================================
	# Deferred evaluation
	
	static func get_where_type(cmps):
		if cmps is FuncRef:
			return WhereFunc
		elif cmps is Operators.OperatorBase:
			return WhereOp
		# if cmps is not a Dictionary; at this point it is not a valid type
		assert(cmps is Dictionary)
		return WhereDict

	func where(cmps):
		var cls = get_where_type(cmps)
		return cls.new(self, cmps)
			
	func project(fields: Array):
		return Project.new(self, fields)
		
	func take(amt):
		return Take.new(self, amt)
		
	func select(obj:Object, func_name:String, args:Array=[]):
		return Select.new(self, funcref(obj, func_name), args)
		
	func skip(count):
		return Skip.new(self, count)
		
				
# Pretty much does the same as the native array but is compatible with other enumerators
class List:
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
		
	func to_array():
		return [] + source
		
	func at(index):
		if index >= 0 and index < len(source):
			return source[index]
		return null
				
	func reset():
		.reset()
		index = -1

# Get all where preds evaluate to true
# This is a base class and should be extended, not used directly
class WhereBase:
	extends Enumerable

	var preds

	func _init(source, preds).(source):
		self.preds = preds

	func get_result(item):
		printerr("Using base where class!")
		return false

	func _iter_next(arg):
		if state == RUNNING:
			while source._iter_next(arg):
				var item = source.current
				if get_result(item):
					current = item
					return true
			reset()
		return false
		
	func _iter_init(arg):
		._iter_init(arg)
		if not source._iter_init(arg): return false
		
		var item = source.current
		if get_result(source.current):
			current = item
			return true

		return _iter_next(arg)
				
# expects comps in the form of {field0=value, field1=ops.eq(value), field2=ops.gt(value)}
# if no operator is provided, the value will be wrapped with Operators.Eq
# Note: this will only work with Lists of Objects or Dictionaries
class WhereDict:
	extends WhereBase

	func _init(source, preds:Dictionary).(source, preds):
		for p in preds:
			if not preds[p] is Operators.OperatorBase:
				preds[p] = Operators.Eq.new(preds[p])

	func get_result(item):
		return Operators.item_valid(item, preds)
				
# expects only a FuncRef. The function taking a single argument and returning a bool
class WhereFunc:
	extends WhereBase
	
	func _init(source, preds:FuncRef).(source, preds):
		pass
		
	func get_result(item):
		return preds.call_func(item)

# expects a class with a function 'eval(item)' that returns a bool
class WhereOp:
	extends WhereBase
	const OpBase = preload("res://addons/fakeo_db/scripts/Operators.gd").OperatorBase
	
	func _init(source, preds:OpBase).(source, preds):
		pass
		
	func get_result(item):
		return preds.eval(item)

								
				
# Returns only the fields matching names in the 'fields' array
# ["name", "id"] => {name="the_name", id=0}
class Project:
	extends Enumerable
	
	var fields
	
	func _init(source, fields:Array).(source):
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

# Take first N items from source
class Skip:
	extends Enumerable
	
	var count: int
	var i: int
	
	func _init(source, count:int).(source):
		self.source = source
		self.count = count
		self.i=-1
		
	func _iter_next(arg):
		if state == RUNNING:
			while source._iter_next(arg):
				i += 1
				if i >= count - 1:
					current = source.current
					return true
			reset()
		return false
		
	func _iter_init(arg):
		._iter_init(arg)
		if not source._iter_init(arg): return false
		
		var item = source.current
		if count == 0:
			current = item
			return true

		return _iter_next(arg)	
			
	func reset():
		.reset()
		i = -1

# Select from source using a given function reference
class Select:
	extends Enumerable
	
	var select_ref
	
	func _init(source, select_ref, args:Array=[]).(source):
		self.select_ref = Operators.FuncOpObjArgs.new(select_ref, args)

	func get_result(item):
		return select_ref.eval(item)
		
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