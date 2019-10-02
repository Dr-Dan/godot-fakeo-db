tool
extends EditorScript

const Op = Operators
const OpFac = OperatorFactory

var data = [
	{name="Wooden Sword", type="melee", subtype="sword", dmg=2, range=1.2},
	{name="Katana", type="melee", subtype="sword", dmg=16, range=1.6},
	{name="Nice Spear", type="melee", subtype="spear", dmg=13, range=2.0},
	{name="Jan's Hammer", type="melee", subtype="blunt", dmg=30, range=1.3},
	{name="John's Rock", type="ranged", subtype="thrown", dmg=21, range=10.0, firing_rate=0.5},
	{name="Wooden Bow", type="ranged", subtype="bow", dmg=4, range=20.0, firing_rate=1.4},
	{name="Glass Bow", type="ranged", subtype="bow", dmg=7, range=20.0, firing_rate=1.7},
	{name="Ancient Bow", type="ranged", subtype="bow", dmg=10, range=30.0, firing_rate=1.0},
]

var list = EnumeratorsAgain.ListEnumerator.new(data)
var where = EnumeratorsAgain.Where.new(list, {dmg=OpFac.gteq(10)})
func _run():
	for i in where:
		print(i)
		