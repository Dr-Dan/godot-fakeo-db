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

	func last(cmps):
		var result = where(cmps).to_array()
		if result.empty():
			 return null
		return result[result.size()-1]
		
	# returns true if any match conditions
	func exists(cmps):
		var d = first(cmps)
		return d != null

	func count():
		return to_array().size()	
				
	# apply function to each item
	func for_each(obj:Object, func_name:String, args:Array=[]):
		return for_each_ref(funcref(obj, func_name), args)
		
	func for_each_ref(fn:FuncRef, args:Array=[]):
		select_ref(fn, args).to_array()		
	# ===============================================================================

	# Deferred evaluation	
	func where(cmps):
		var cls = WhereUtil.get_where_type(cmps)
		assert(cls != null) # no Where type for input
		return cls.new(self, cmps)
		
	func where_ref(fn:FuncRef, args:Array=[]):
		return WhereFunc.new(self, fn, args)		
			
	func project(fields: Array):
		return Project.new(self, fields)
		
	func take(amt):
		return Take.new(self, amt)
		
	func select(obj:Object, func_name:String, args:Array=[]):
		return select_ref(funcref(obj, func_name), args)
	
	func select_ref(fn:FuncRef, args:Array=[]):
		return Select.new(self, fn, args)
		
	func as_args(obj:Object, func_name:String):
		return as_args_ref(funcref(obj, func_name))

	func as_args_ref(fn:FuncRef):
		return AsArgs.new(self, fn)
		
	func skip(count):
		return Skip.new(self, count)
		
		
		
class WhereUtil:
	extends Resource
	
	static func get_where_type(cmps):
		if cmps is FuncRef:
			return WhereFunc
		elif cmps is Operators.OperatorBase:
			return WhereOp
		# if cmps is not a Dictionary; at this point it is not a valid type
		assert(cmps is Dictionary)
		return WhereDict


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

					
# Get all where preds evaluate to true
# This is a base class and should be extended, not used directly
class WhereBase:
	extends Enumerable


	func _init(source).(source):
		pass
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
# Note: this will only work over Lists containing Objects and/or Dictionaries
class WhereDict:
	extends WhereBase
	var preds

	func _init(source, preds:Dictionary).(source):
		self.preds = preds
		for p in preds:
			if not preds[p] is Operators.OperatorBase:
				preds[p] = Operators.Eq.new(preds[p])

	func get_result(item):
		return Operators.item_valid(item, preds)
				
# expects only a FuncRef. Will return appropriate class for number of args (max 3)
class WhereFunc:
	extends WhereBase
	var pred_func
	func _init(source, preds:FuncRef, args:Array=[]).(source):
		self.pred_func = Operators.FuncOpUtil.get_func_op(preds, args)
				
	func get_result(item):
		return pred_func.eval(item)

# expects a class with a function 'eval(item)' that returns a bool
class WhereOp:
	extends WhereBase
	var pred_op
	const OpBase = preload("res://addons/fakeo_db/scripts/Operators.gd").OperatorBase
	
	func _init(source, pred_op:OpBase).(source):
		self.pred_op = pred_op
		
	func get_result(item):
		return pred_op.eval(item)


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
		self.select_ref = Operators.FuncOpUtil.get_func_op(select_ref, args)

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

# Expects each 'item' to be an array of arguments (up to 3) to a function
class AsArgs:
	extends Enumerable

	var select_ref

	func _init(source, select_ref).(source):
		self.select_ref = select_ref

	func get_result(item):
		assert(item is Array)
		item = [] + item
		var i = item.pop_front()
		var func_op = Operators.FuncOpUtil.get_func_op(select_ref, item)
		return func_op.eval(i)
	
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
		
	func run():
		to_array()
		