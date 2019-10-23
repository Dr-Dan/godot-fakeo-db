extends Node

enum ItemType{
	NONE,
	MEAT,
	AXE,
	SWORD,
	SPELL_HEAL,
	SCROLL_FIRE,
	THE_JUICE,
	GOLD_PLATE,
}

export (ItemType) var type = ItemType.NONE
export (int) var amt

func setup(type, amt):
	self.type = type
	self.amt = amt
	return self