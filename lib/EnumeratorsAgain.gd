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
		if state == START:
			if not _iter_init(arg): return false
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
	var source: ListEnumerator
	
	func _init(source, preds).():
		self.preds = preds
		self.source = source

	func _iter_next(arg):
		if state == START:
			if not _iter_init(arg) or not source._iter_init(arg): return false
		if state == RUNNING:
			while source._iter_next(arg):
				var item = source.current
				for key in preds:
					if key in item and preds[key].eval(item[key]):
						current = item
						return true
			reset()
		return false
		
	func _iter_init(arg):
		._iter_init(arg)
		if _iter_next(arg):
			return true
		return false
				