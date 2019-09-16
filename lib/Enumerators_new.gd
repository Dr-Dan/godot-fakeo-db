extends Resource
class_name Enumerators_new

const NONE = 0
const COMPLETE = 2
const RUNNING = 1

class Enumerator_new:
#	extends "IterList.gd"
	var start
	var curr
	var end
	var increment
	var items
	var status
	var result

	func _init(items=[]):
		status = NONE
		result = null # seems unnecessary but will be useful for Select
		increment = 1
		start = 0
		curr = start
		self.items = items
		self.end = items.size()
        
    # TODO: iterator shouldn't have size
	func size():
		return items.size()

    # NOTE: this may be overridden for child classes
	func should_continue():
		return status == RUNNING and result != null

	func _iter_init(arg):
		status = COMPLETE
		curr = start
		result = items[curr]
		return should_continue()

	func _iter_next(arg):
		status = COMPLETE
		curr += 1
		result = items[curr]
		return should_continue()

    # TODO: return next evaluated item
	func _iter_get(arg):
		return result
		
	func to_list():
		var arg = null
		var r = []
		_iter_init(arg)
		var next = _iter_get(arg)
		while next != null:
			r.append(next)
			_iter_next(arg)
			next = _iter_get(arg)
		return r

    # TODO: to list
	func where(cmps):
		return Where.new(cmps, self)
			
	func project(fields: Array):
		return Project.new(fields, self)
		
	func take(amt):
		return Take.new(amt, self)
		
class Where:
	extends Enumerator_new
	var comps = {}

	func _init(comps: Dictionary, items).(items):
		self.comps = comps

	func item_valid(item):
		for key in comps:
			if key in item:
				if not comps[key].eval(item[key]):
					return false
		return true
			
	func advance(arg, init=false):
		var item = null
		if init:
			if items._iter_init(arg):
				item = items._iter_get(arg)
				if item_valid(item):
					result = item
					return true
			else:
				result = null
				return false
				
		while true:
			if items._iter_next(arg):
				item = items._iter_get(arg)
			else:
				break
			if item_valid(item):
				result = item
				return true

		result = null
		return false
		
				
	func _iter_init(arg):
		status = RUNNING
		curr = start
		if not advance(arg, true):
			status = COMPLETE
		return should_continue()

	func _iter_next(arg):
		if not advance(arg):
			status = COMPLETE
		return should_continue()
		
class Take:
	extends Enumerator_new
	var amt

	func _init(amt: int, items).(items):
		self.amt = amt

	func _iter_init(arg):
		status = RUNNING
		curr = start
		if items._iter_init(arg):
			result = items._iter_get(arg)
		else:
			status = COMPLETE

		return should_continue()

	func _iter_next(arg):
		curr += 1
		if curr < amt and items._iter_next(arg):
			result = items._iter_get(arg)
		else:
			status = COMPLETE

		return should_continue()
		
class Project:
	extends Enumerator_new
	var fields
	
	func _init(fields: Array, items).(items):
		self.fields = fields

	func get_result(item):
		var n = {}
		for f in fields:
			if f in item:
				n[f] = item[f]
		return n
		
	func advance(arg, init=false):
		var item = null
		if init:
			if items._iter_init(arg):
				item = items._iter_get(arg)
				result = get_result(item)
				return true
			else:
				result = null
				return false
					
		while true:
			if items._iter_next(arg):
				item = items._iter_get(arg)
				result = get_result(item)
				return true
			else:
				break

		result = null
		return false
		
				
	func _iter_init(arg):
		status = RUNNING
		curr = start
		if not advance(arg, true):
			status = COMPLETE
		return should_continue()

	func _iter_next(arg):
		if not advance(arg):
			status = COMPLETE
		return should_continue()