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
	func first(cmps):
		return at(0)

	func last(cmps):
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
		if source_valid(data):
			if data is Enumerable:
				source = data
			elif data is Array:
				source = List.new(data)
			return source
		return null

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
	

# ===============================================================================
# FILTERS

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
		
# expects a class with a function 'eval(item)' that returns a bool
class WhereOp:
	extends WhereBase
	const OpBase = preload("res://addons/fakeo_db/scripts/Operators.gd").OperatorBase

	var pred_op
	
	func _init(source, pred_op:OpBase).(source):
		self.pred_op = pred_op
		
	func get_result(item):
		return pred_op.eval(item)

# expects comps in the form of {field0=value, field1=ops.eq(value), field2=ops.gt(value)}
# if no operator is provided, the value will be wrapped with Operators.Eq
class WhereDict:
	extends WhereOp
	# var preds

	func _init(source, preds:Dictionary).\
		(source, Operators.DictCompare.new(preds)):
			pass

# expects a FuncRef and anyarguments as an array.
class WhereFunc:
	extends WhereOp
	func _init(source, preds:FuncRef, args:Array=[]).\
		(source, Operators.Func.new(preds, args)):
		pass

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

# ===============================================================================
# MAPPERS

class SelectOp:
	extends Enumerable
	
	var select_op
	
	func _init(source, select_op).(source):
		self.select_op = select_op

	func get_result(item):
		return select_op.eval(item)
		
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
		
class Select:
	extends SelectOp
	
	func _init(source, select_fn, args:Array=[]).(source, Operators.Func.new(select_fn, args)):
		pass

class Project:
	extends SelectOp

	func _init(source, fields:Array).\
		(source, Operators.OpenMulti.new(fields)):
		pass
#
class ProjectDeep:
	extends SelectOp

	func _init(source, fields:Array).(source, Operators.OpenMultiDeep.new(fields)):
		pass
