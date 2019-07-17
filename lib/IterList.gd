#extends Resource
class_name IterList

var start 
var curr
var end
var increment 
var items = []

func _init(items):
	increment = 1
	start = 0
	curr = start
	self.items = items
	self.end = items.size()
	
func size():
	return items.size()

func is_done():
	return (curr < end)

func do_step():
	curr += increment
	return is_done()

func _iter_init(arg):
	curr = start
	return is_done()

func _iter_next(arg):
	return do_step()

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
	

func where(cmps) -> IterList:
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
			if key in item:
				if cmps[key].eval(item[key]):
					result += 1
	return result
		
func select(fields: Array):
	var result = []
	for item in items:
		var n = {}
		for f in fields:
#			if cmps.empty():
			n[f] = item[f]
#			continue
								
#			for key in cmps:
#				if key in item:
#					if f in item\
#					and (cmps[key].eval(item[key])):
#						n[f] = item[f]
		if not n.empty():
			result.append(n)
	return get_script().new(result)
	

func first(cmps):
	for item in items:
		for key in cmps:
			if key in item:
				if cmps[key].eval(item[key]):
					return item
	return null
	
func to_list(deep=false):
	return items.duplicate(deep)	
	

#func select_grouped(cmps_dict, fields: Array):
#	var result = {}
#	for k in cmps_dict:
#		result[k] = []
#
#	for item in items:
#		for result_key in cmps_dict:
#			var data = {}
#			var comp_dict = cmps_dict[result_key]
#
#			for comp_k in comp_dict:
#				if comp_k in item:
#
#					for f in fields:
#						if f in item\
#						and comp_dict[comp_k].eval(item[comp_k]):
#							data[f] = item[f]
#			if not data.empty():
#				result[result_key].append(data)
#
#	for k in cmps_dict:
#		result[k] = get_script().new(result[k])
#
#	return result
	

#func where_k(cmps):
#	var result = []
#	for item in items:
#		for key in cmps:
#			if cmps[key].eval(item[key]):
#				result.append(item)
#	return result

#func first_k(cmps):
#	for item in items:
#		for key in cmps:
#			if cmps[key].eval(key):
#				return item
#	return null

