extends Resource
class_name GodotLinqFactory

static func and_(items: Array):
	return GodotLINQ.and_.new(items)

static func or_(items: Array):
	return GodotLINQ.or_.new(items)

static func not_(item):
	return GodotLINQ.not_.new(item)
	
static func in_(item):
	return GodotLINQ.in_.new(item)

static func lt(item):
	return GodotLINQ.lt.new(item)
		
static func gt(item):
	return GodotLINQ.gt.new(item)

static func eq(item):
	return GodotLINQ.eq.new(item)


static func op(item:String, arg):
	match item:
		"<":
			return lt(arg)
		">":
			return gt(arg)
		"&", "and":
			return and_(arg)
		"=":
			return eq(arg)
		"not", "!":
			return not_(arg)
		"or", "|":
			return or_(arg)
		"in":
			return in_(arg)
	return null
		