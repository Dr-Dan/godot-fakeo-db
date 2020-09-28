static func pr_equals_break(txt:String=''):
	print("%s\n###############################" % txt)

static func pr_dash_break(txt:String=''):
	print("%s\n--------------" % txt)

static func pr_array(txt:String='', result=[]):
	pr_dash_break()
	print('%s\n' % txt)
	for i in result:
		print(i)
