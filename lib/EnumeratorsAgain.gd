class_name EnumeratorsAgain

class Enumerable:
	const START = 0
	const RUNNING = 1

	var state: int
	var current

	func _init():
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
				
class ListEnumerator:
	extends Enumerable
	var index
	var source
	
	func _init(source).():
		index = -1
		self.source = source

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
		current = source[index]
		return true
		
	func reset():
		.reset()
		index = -1

class Where:
	extends Enumerable

	var preds
	var source
	
	func _init(source, preds).():
		self.preds = preds
		self.source = source

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
		source._iter_init(arg)		
		
		var item = source.current
		if Operators.item_valid(item, preds):
			current = item
			return true

		return _iter_next(arg)
				
class Project:
	extends Enumerable
	
	var fields
	var source
	
	func _init(source, fields).():
		self.fields = fields
		self.source = source
	
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
		source._iter_init(arg)		
		current = get_result(source.current)
		return true
		
class Take:
	extends Enumerable
	
	var count: int
	var i: int
	var source
	
	func _init(source, count:int).():
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
		source._iter_init(arg)		
		
		i = 0
		current = source.current
		return true
								
	func reset():
		.reset()
		i = -1