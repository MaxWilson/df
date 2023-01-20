type MagicType = Clerical | Wizardly | Druidic

type Prereq = {
    heading: MagicType * string
    items: string list
    }

type Spell = {
    name: string
    college: string
    prereqs: Prereq list
    page: int
    }

let spells = [
    { name = "Affect Spirits"
      college = "Necro."
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }]
      page = 59 }
    { name = "Agonize"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M2"; "Sensitize"] }]
      page = 20 }
    { name = "Air Jet"
      college = "Air"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Shape Air"] }]
      page = 15 }
    { name = "Alertness"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["2 Keen Sense spells"] }]
      page = 53 }
    { name = "Ambidexterity"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Grace"] }]
      page = 20 }
    { name = "Analyze Magic"
      college = "Know."
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Identify Spell"] }]
      page = 42 }
    { name = "Animal Control"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }]
      page = 17 }
    { name = "Apportation"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"] }]
      page = 56 }
    { name = "Arboreal Immurement"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI5"] }]
      page = 61 }
    { name = "Armor"
      college = "P&W"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["Shield"] }]
      page = 63 }
    { name = "Astral Block"
      college = "Necro."
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI4"] }; { heading = (Wizardly, "W")
                               items = ["Repel Spirits"; "Summon Spirit"] }]
      page = 59 }
    { name = "Astral Vision"
      college = "Know./Necro."
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }; { heading = (Wizardly, "W*")
                                        items = ["Sense Spirit"; "See Invisible"] }]
      page = 42 }
    { name = "Aura"
      college = "Know."
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }; { heading = (Wizardly, "W*")
                                        items = ["Detect Magic"] }]
      page = 42 }
    { name = "Awaken"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }]
      page = 36 }
    { name = "Balance"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Grace"] }]
      page = 20 }
    { name = "Banish"
      college = "Necro."
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI4"] }; { heading = (Wizardly, "W")
                               items = ["M1"; "1 spell from 10 colleges"] }]
      page = 59 }
    { name = "Beast Link"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }]
      page = 18 }
    { name = "Beast Possession"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI4"] }]
      page = 18 }
    { name = "Beast-Rouser"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI1"] }]
      page = 18 }
    { name = "Beast Seeker"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }]
      page = 18 }
    { name = "Beast-Soother"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI1"] }]
      page = 18 }
    { name = "Beast Speech"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }]
      page = 18 }
    { name = "Beast Summoning"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }]
      page = 18 }
    { name = "Blackout"
      college = "L&D"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Darkness"] }]
      page = 46 }
    { name = "Bladeturning"
      college = "P&W"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Shield"] }]
      page = 63 }
    { name = "Bless"
      college = "Meta"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI5"] }]
      page = 50 }
    { name = "Blink"
      college = "Gate/Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M3"; "IQ 13+"; "1 spell from 10 colleges"] }]
      page = 56 }
    { name = "Blink Other"
      college = "Gate/Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Blink"] }]
      page = 56 }
    { name = "Blur"
      college = "L&D"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Darkness"] }]
      page = 46 }
    { name = "Borrow Language"
      college = "C&E"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Lend Language"] }]
      page = 24 }
    { name = "Borrow Skill"
      college = "C&E"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Lend Skill"] }]
      page = 24 }
    { name = "Bravery"
      college = "Mind"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }; { heading = (Wizardly, "W*")
                                        items = ["Fear"] }]
      page = 53 }
    { name = "Breathe Water"
      college = "Air/Water"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI3"] }; { heading = (Druidic, "D")
                               items = ["PI3"] };
        { heading = (Wizardly, "W")
          items = ["Create Air"; "Destroy Water"] }]
      page = 68 }
    { name = "Bright Vision"
      college = "L&D"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Keen Vision or 5 L&D spells"] }]
      page = 46 }
    { name = "Burning Touch"
      college = "Fire"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M2"; "6 Fire spells including Heat"] }]
      page = 29 }
    { name = "Charm"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M1/BT1"; "Loyalty"; "7 other Mind Control spells"] }]
      page = 53 }
    { name = "Cleansing"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }]
      page = 36 }
    { name = "Climbing"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M0"] }]
      page = 20 }
    { name = "Clumsiness"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Spasm"] }]
      page = 20 }
    { name = "Cold"
      college = "Fire"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Heat"] }]
      page = 29 }
    { name = "Colors"
      college = "L&D"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Light"] }]
      page = 46 }
    { name = "Command"
      college = "Mind"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }; { heading = (Wizardly, "W*")
                                        items = ["M2/BT2"; "Forgetfulness"] }]
      page = 53 }
    { name = "Command Spirit"
      college = "Necro."
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }; { heading = (Wizardly, "W")
                                        items = ["Summon Spirit"; "Turn Spirit"] }]
      page = 60 }
    { name = "Compel Truth"
      college = "C&E"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }; { heading = (Wizardly, "W*")
                                        items = ["M2/BT2"; "Truthsayer"] }]
      page = 24 }
    { name = "Complex Illusion"
      college = "Illusion"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Simple Illusion"; "Sound"] }]
      page = 40 }
    { name = "Conceal"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }]
      page = 61 }
    { name = "Concussion"
      college = "Air/Sound"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Shape Air"; "Thunderclap"] }]
      page = 15 }
    { name = "Continual Light"
      college = "L&D"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["Light"] }]
      page = 46 }
    { name = "Control Gate"
      college = "Gate"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M3"; "Seek Gate"] }]
      page = 34 }
    { name = "Control Illusion"
      college = "Illusion"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Perfect Illusion"] }]
      page = 40 }
    { name = "Control Person"
      college = "C&E"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Soul Rider or Telepathy"] }]
      page = 24 }
    { name = "Cook"
      college = "Food"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Test Food"; "Create Fire"] }]
      page = 32 }
    { name = "Coolness"
      college = "P&W/Water"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI1"] }; { heading = (Druidic, "D")
                               items = ["PI1"] }; { heading = (Wizardly, "W")
                                                    items = ["Cold"] }]
      page = 68 }
    { name = "Copy"
      college = "M&B"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["5 M&B spells including Restore"; "no Illiteracy"] }]
      page = 48 }
    { name = "Counterspell"
      college = "Meta"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"] }]
      page = 51 }
    { name = "Create Air"
      college = "Air"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Purify Air"] }]
      page = 16 }
    { name = "Create Animal"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI4"] }]
      page = 19 }
    { name = "Create Earth"
      college = "Earth"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Earth to Stone"] }]
      page = 27 }
    { name = "Create Fire"
      college = "Fire"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Ignite Fire or Seek Fire"] }]
      page = 29 }
    { name = "Create Food"
      college = "Food"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }; { heading = (Wizardly, "W")
                                        items = ["Cook"; "Seek Food"] }]
      page = 32 }
    { name = "Create Plant"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }]
      page = 61 }
    { name = "Create Water"
      college = "Water"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["Purify Water"] }]
      page = 68 }
    { name = "Cure Disease"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }; { heading = (Druidic, "D")
                                        items = ["PI2"] }]
      page = 36 }
    { name = "Curse"
      college = "Meta"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI5"] }]
      page = 51 }
    { name = "Dark Vision"
      college = "L&D"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Infravision or Night Vision"] }]
      page = 46 }
    { name = "Darkness"
      college = "L&D"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Continual Light"] }]
      page = 46 }
    { name = "Daze"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Foolishness"] }]
      page = 54 }
    { name = "Death Vision"
      college = "Necro."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"] }]
      page = 60 }
    { name = "Deathtouch"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Wither Limb"] }]
      page = 20 }
    { name = "Debility"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M0"] }]
      page = 20 }
    { name = "Decay"
      college = "Food"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Test Food"] }]
      page = 32 }
    { name = "Deflect Energy"
      college = "Fire"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"; "Shape Fire"] }]
      page = 29 }
    { name = "Deflect Missile"
      college = "Move./P&W"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Apportation"] }]
      page = 56 }
    { name = "Dehydrate"
      college = "Water"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["5 Water spells including Destroy Water"] }]
      page = 68 }
    { name = "Delayed Message"
      college = "Sound"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M1/BT1"; "Sense Life"; "Voices"] }]
      page = 66 }
    { name = "Destroy Air"
      college = "Air"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Create Air"] }]
      page = 16 }
    { name = "Destroy Water"
      college = "Water"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Create Water"] }]
      page = 68 }
    { name = "Detect Magic"
      college = "Know."
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI1"] }; { heading = (Druidic, "D")
                               items = ["PI1"] }; { heading = (Wizardly, "W*")
                                                    items = ["M1/BT1"] }]
      page = 43 }
    { name = "Detect Poison"
      college = "Healing/P&W"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }; { heading = (Druidic, "D")
                                        items = ["PI1"] }]
      page = 36 }
    { name = "Dispel Illusion"
      college = "Illusion"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Control Illusion"] }]
      page = 40 }
    { name = "Dispel Magic"
      college = "Meta"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI4"] }; { heading = (Druidic, "D")
                               items = ["PI4"] };
        { heading = (Wizardly, "W")
          items = ["Counterspell"; "any 12 other spells"] }]
      page = 51 }
    { name = "Dispel Possession"
      college = "C&E"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }]
      page = 24 }
    { name = "Divert Teleport"
      college = "Gate/Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M3"; "Trace Teleport"] }]
      page = 34 }
    { name = "Drunkenness"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Foolishness"; "Clumsiness"] }]
      page = 54 }
    { name = "Dull Sense"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M0/BT1"] }]
      page = 54 }
    { name = "Dullness"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["2 Dull Sense spells"] }]
      page = 54 }
    { name = "Earth to Air"
      college = "Air/Earth"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Create Air"; "Shape Earth"] }]
      page = 16 }
    { name = "Earth to Stone"
      college = "Earth"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"; "Shape Earth"] }]
      page = 27 }
    { name = "Earth Vision"
      college = "Earth/Know."
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }; { heading = (Wizardly, "W*")
                                        items = ["Shape Earth"] }]
      page = 27 }
    { name = "Earthquake"
      college = "Earth"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI5"] }; { heading = (Druidic, "D")
                                        items = ["PI6"] }]
      page = 27 }
    { name = "Entombment"
      college = "Earth"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI5"] }; { heading = (Wizardly, "W")
                                        items = ["M2"; "5 Earth spells"] }]
      page = 27 }
    { name = "Entrap Spirit"
      college = "Necro."
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI5"] };
        { heading = (Wizardly, "W")
          items = ["M1"; "7 Necromantic spells including Turn Spirit"] }]
      page = 60 }
    { name = "Essential Food"
      college = "Food"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI4"] }; { heading = (Wizardly, "W")
                               items = ["6 Food spells including Create Food"] }]
      page = 32 }
    { name = "Ethereal Body"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M3"; "6 Movement spells"] }]
      page = 57 }
    { name = "Explosive Fireball"
      college = "Fire"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Fireball"] }]
      page = 29 }
    { name = "Explosive Lightning"
      college = "Air/Weather"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Lightning"] }]
      page = 71 }
    { name = "Extinguish Fire"
      college = "Fire"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["Ignite Fire"] }]
      page = 30 }
    { name = "Far-Feeling"
      college = "Know."
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M1/BT1"] }]
      page = 43 }
    { name = "Far-Hearing"
      college = "Know./Sound"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M1/BT1"; "4 Sound spells"] }]
      page = 66 }
    { name = "Far-Tasting"
      college = "Food/Know."
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M1"; "Seek Food"] }]
      page = 33 }
    { name = "Fascinate"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Daze"] }]
      page = 54 }
    { name = "Fasten"
      college = "M&B"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Knot"] }]
      page = 48 }
    { name = "Fear"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Sense Emotion"] }]
      page = 54 }
    { name = "Final Rest"
      college = "Healing/Necro."
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }]
      page = 36 }
    { name = "Find Direction"
      college = "Know."
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI1"] }; { heading = (Wizardly, "W*")
                                        items = ["M1/BT1"] }]
      page = 43 }
    { name = "Find Weakness"
      college = "M&B"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["1 Air"; "1 Earth"; "1 Fire"; "1 Water spell"] }]
      page = 49 }
    { name = "Fire Cloud"
      college = "Fire"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Fireball"; "Shape Air"] }]
      page = 30 }
    { name = "Fireball"
      college = "Fire"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"; "Create Fire"; "Shape Fire"] }]
      page = 30 }
    { name = "Fireproof"
      college = "Fire"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["Extinguish Fire"] }]
      page = 30 }
    { name = "Flame Jet"
      college = "Fire"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Create Fire"; "Shape Fire"] }]
      page = 30 }
    { name = "Flaming Missiles"
      college = "Fire"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Flaming Weapon"] }]
      page = 30 }
    { name = "Flaming Weapon"
      college = "Fire"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }; { heading = (Wizardly, "W")
                                        items = ["M2"; "Heat"] }]
      page = 31 }
    { name = "Flesh to Stone"
      college = "Earth"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Earth to Stone"] }]
      page = 27 }
    { name = "Flight"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M2"; "Levitation"] }]
      page = 57 }
    { name = "Fog"
      college = "Water/Weather"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }]
      page = 71 }
    { name = "Foolishness"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M0/BT1"; "IQ 12+"] }]
      page = 54 }
    { name = "Forest Warning"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }]
      page = 61 }
    { name = "Forgetfulness"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M1/BT1"; "Foolishness"] }]
      page = 54 }
    { name = "Frailty"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Lend Energy"] }]
      page = 21 }
    { name = "Freeze"
      college = "Water"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }; { heading = (Wizardly, "W")
                                        items = ["Shape Water"] }]
      page = 68 }
    { name = "Frostbite"
      college = "Water"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI4"] }; { heading = (Wizardly, "W")
                                        items = ["Cold"; "Freeze"] }]
      page = 69 }
    { name = "Garble"
      college = "Sound"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Voices"] }]
      page = 66 }
    { name = "Geyser"
      college = "Water"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI6"] }]
      page = 69 }
    { name = "Gift of Letters"
      college = "C&E"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI4"] }; { heading = (Wizardly, "W*")
                               items = ["Borrow Language"; "3 written languages"] }]
      page = 24 }
    { name = "Gift of Tongues"
      college = "C&E"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI4"] }; { heading = (Wizardly, "W*")
                               items = ["Borrow Language"; "3 spoken languages"] }]
      page = 24 }
    { name = "Glass Wall"
      college = "Know."
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["5 Knowledge spells or Earth Vision"] }]
      page = 43 }
    { name = "Glow"
      college = "L&D"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["Continual Light"] }]
      page = 46 }
    { name = "Glue"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Haste"] }]
      page = 57 }
    { name = "Grace"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Clumsiness"] }]
      page = 21 }
    { name = "Grease"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Haste"] }]
      page = 57 }
    { name = "Great Haste"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"; "IQ 12+"; "Haste"] }]
      page = 57 }
    { name = "Great Healing"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }]
      page = 37 }
    { name = "Great Voice"
      college = "Sound"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }; { heading = (Wizardly, "W*")
                                        items = ["Thunderclap"; "Voices"] }]
      page = 66 }
    { name = "Great Ward"
      college = "Meta"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M2"; "Ward"] }]
      page = 51 }
    { name = "Hail"
      college = "Water/Weather"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI4"] }]
      page = 71 }
    { name = "Haste"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M0"] }]
      page = 57 }
    { name = "Hawk Vision"
      college = "L&D"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["Keen Vision or 5 L&D spells"] }]
      page = 47 }
    { name = "Healing Slumber"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }]
      page = 37 }
    { name = "Heat"
      college = "Fire"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Create Fire"; "Shape Fire"] }]
      page = 31 }
    { name = "Hide"
      college = "L&D"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Blur or Forgetfulness"] }]
      page = 47 }
    { name = "Hide Emotion"
      college = "C&E"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Sense Emotion"] }]
      page = 24 }
    { name = "Hide Path"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }]
      page = 62 }
    { name = "Hide Thoughts"
      college = "C&E"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }; { heading = (Wizardly, "W*")
                                        items = ["Truthsayer or Hide Emotion"] }]
      page = 25 }
    { name = "Hinder"
      college = "Body/Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Clumsiness or Haste"] }]
      page = 21 }
    { name = "History"
      college = "Know."
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Trace"] }]
      page = 43 }
    { name = "Hold Breath"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"; "Vigor"] }]
      page = 21 }
    { name = "Hush"
      college = "Sound"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Silence"] }]
      page = 66 }
    { name = "Hybrid Control"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }]
      page = 19 }
    { name = "Ice Dagger"
      college = "Water"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Ice Sphere or Water Jet"] }]
      page = 69 }
    { name = "Ice Sphere"
      college = "Water"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Shape Water"] }]
      page = 69 }
    { name = "Icy Missiles"
      college = "Water"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Icy Weapon"] }]
      page = 69 }
    { name = "Icy Weapon"
      college = "Water"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Create Water"] }]
      page = 69 }
    { name = "Identify Plant"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI1"] }]
      page = 62 }
    { name = "Identify Spell"
      college = "Know."
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Detect Magic"] }]
      page = 43 }
    { name = "Ignite Fire"
      college = "Fire"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M0"] }]
      page = 31 }
    { name = "Illusion Disguise"
      college = "Illusion"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Simple Illusion"] }]
      page = 40 }
    { name = "Illusion Shell"
      college = "Illusion"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Simple Illusion"] }]
      page = 41 }
    { name = "Independence"
      college = "Illusion"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Simple Illusion"] }]
      page = 41 }
    { name = "Infravision"
      college = "L&D"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Keen Vision or 5 L&D spells"] }]
      page = 47 }
    { name = "Initiative"
      college = "Illusion"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Independence"; "Wisdom"] }]
      page = 41 }
    { name = "Instant Neutralize Poison"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI4"] }; { heading = (Druidic, "D")
                                        items = ["PI3"] }]
      page = 37 }
    { name = "Invisibility"
      college = "L&D"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["6 L&D spells including Blur"] }]
      page = 47 }
    { name = "Iron Arm"
      college = "P&W"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Resist Pain"; "DX 11+"] }]
      page = 64 }
    { name = "Itch"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M0"] }]
      page = 21 }
    { name = "Keen Hearing"
      college = "Sound"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M0/BT1"] }]
      page = 54 }
    { name = "Keen Sense"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M0/BT1"] }]
      page = 54 }
    { name = "Knot"
      college = "M&B"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Stiffen"] }]
      page = 49 }
    { name = "Know Illusion"
      college = "Illusion"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Simple Illusion"] }]
      page = 41 }
    { name = "Know Location"
      college = "Know."
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }; { heading = (Wizardly, "W*")
                                        items = ["M1/BT1"; "Tell Position"] }]
      page = 43 }
    { name = "Lend Energy"
      college = "Healing/Meta"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["M1"] }]
      page = 37 }
    { name = "Lend Language"
      college = "C&E"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["3 C&E spells"] }]
      page = 25 }
    { name = "Lend Skill"
      college = "C&E"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Mind-Sending"; "IQ 11+"] }]
      page = 25 }
    { name = "Lend Vitality"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }]
      page = 37 }
    { name = "Levitation"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Apportation"] }]
      page = 57 }
    { name = "Light"
      college = "L&D"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["M0"] }]
      page = 47 }
    { name = "Light Jet"
      college = "L&D"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["Continual Light"] }]
      page = 47 }
    { name = "Light Tread"
      college = "Move."
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["Apportation"; "Shape Earth"] }]
      page = 57 }
    { name = "Lighten Burden"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Apportation"] }]
      page = 58 }
    { name = "Lightning"
      college = "Air/Weather"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI4"] }; { heading = (Wizardly, "W")
                                        items = ["M1"; "6 Air spells"] }]
      page = 71 }
    { name = "Lightning Missiles"
      college = "Air/Weather"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Lightning Weapon"] }]
      page = 72 }
    { name = "Lightning Weapon"
      college = "Air/Weather"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M2"; "Lightning"] }]
      page = 72 }
    { name = "Lockmaster"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Either M2"; "Apportation or Locksmith"] }]
      page = 58 }
    { name = "Locksmith"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Apportation"] }]
      page = 58 }
    { name = "Loyalty"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Bravery"; "2 Mind Control spells"] }]
      page = 55 }
    { name = "Mage Sight"
      college = "Know."
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Detect Magic"] }]
      page = 44 }
    { name = "Mage-Stealth"
      college = "Sound"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Hush"] }]
      page = 66 }
    { name = "Magelock"
      college = "P&W"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"] }]
      page = 64 }
    { name = "Magic Resistance"
      college = "Meta"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI3"] }; { heading = (Wizardly, "W")
                               items = ["M1"; "1 spell from 7 different colleges"] }]
      page = 51 }
    { name = "Major Healing"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }]
      page = 37 }
    { name = "Manipulate"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Locksmith"] }]
      page = 58 }
    { name = "Mapmaker"
      college = "M&B"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Copy"; "Measurement"] }]
      page = 49 }
    { name = "Mass Daze"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Daze"; "IQ 13+"] }]
      page = 55 }
    { name = "Mass Sleep"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Sleep"; "IQ 13+"] }]
      page = 55 }
    { name = "Master"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI1"] }]
      page = 19 }
    { name = "Measurement"
      college = "Know."
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M0/BT1"] }]
      page = 44 }
    { name = "Message"
      college = "C&E/Sound"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Great Voice"; "Seeker"] }]
      page = 67 }
    { name = "Might"
      college = "Body"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["Lend Energy"] }]
      page = 21 }
    { name = "Mind-Reading"
      college = "C&E"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Truthsayer or Borrow Language"] }]
      page = 25 }
    { name = "Mind-Search"
      college = "C&E"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Mind-Reading"] }]
      page = 25 }
    { name = "Mind-Sending"
      college = "C&E"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Mind-Reading"] }]
      page = 25 }
    { name = "Minor Healing"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }]
      page = 37 }
    { name = "Mirror"
      college = "L&D"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Colors"] }]
      page = 47 }
    { name = "Missile Shield"
      college = "P&W"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Apportation or Shield"] }]
      page = 64 }
    { name = "Monk’s Banquet"
      college = "Food"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI4"] }]
      page = 33 }
    { name = "Mystic Mist"
      college = "P&W"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["M1"; "Watchdog or Shield"] }]
      page = 64 }
    { name = "Nauseate"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["5 Body Control spells"] }]
      page = 21 }
    { name = "Neutralize Poison"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }; { heading = (Druidic, "D")
                                        items = ["PI2"] }]
      page = 37 }
    { name = "Night Vision"
      college = "L&D"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Keen Vision or 5 L&D spells"] }]
      page = 47 }
    { name = "Nightingale"
      college = "P&W"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Sense Danger"] }]
      page = 64 }
    { name = "No-Smell"
      college = "Air"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["Purify Air"] }]
      page = 16 }
    { name = "Noise"
      college = "Sound"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Wall of Silence"] }]
      page = 67 }
    { name = "Pain"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Spasm"] }]
      page = 21 }
    { name = "Panic"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Fear"] }]
      page = 55 }
    { name = "Paralyze Limb"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"; "5 Body Control spells including Clumsiness"] }]
      page = 21 }
    { name = "Pathfinder"
      college = "Know."
      prereqs =
       [{ heading = (Druidic, "D")
          items = ["PI2"] }; { heading = (Wizardly, "W*")
                               items = ["M1/BT1"; "IQ 12+"; "2 “Seek” spells"] }]
      page = 44 }
    { name = "Pentagram"
      college = "Meta"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI5"] }; { heading = (Wizardly, "W")
                                        items = ["Spell Shield"] }]
      page = 51 }
    { name = "Perfect Illusion"
      college = "Illusion"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"; "Complex Illusion"] }]
      page = 41 }
    { name = "Persuasion"
      college = "C&E"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }; { heading = (Wizardly, "W*")
                                        items = ["Sense Emotion"] }]
      page = 25 }
    { name = "Phantom"
      college = "Illusion"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M2"; "Apportation"; "Hinder"; "Perfect Illusion"] }]
      page = 41 }
    { name = "Phase"
      college = "Gate"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M3"; "Ethereal Body"] }]
      page = 34 }
    { name = "Phase Other"
      college = "Gate"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Phase"] }]
      page = 34 }
    { name = "Plant Control"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }]
      page = 62 }
    { name = "Plant Sense"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }]
      page = 62 }
    { name = "Plant Speech"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }]
      page = 62 }
    { name = "Plant Vision"
      college = "Know./Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }]
      page = 62 }
    { name = "Poison Food"
      college = "Food"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Purify Food"] }]
      page = 33 }
    { name = "Pollen Cloud"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }]
      page = 62 }
    { name = "Possession"
      college = "C&E"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M1/BT1"; "Control Person"] }]
      page = 25 }
    { name = "Prepare Game"
      college = "Food"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Purify Food"] }]
      page = 33 }
    { name = "Projection"
      college = "Know."
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Sense Spirit"; "4 Knowledge spells"] }]
      page = 44 }
    { name = "Protect Animal"
      college = "Animal/P&W"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }]
      page = 19 }
    { name = "Protection from Evil"
      college = "Meta/P&W"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI2"] }; { heading = (Druidic, "D")
                               items = ["PI2"] };
        { heading = (Wizardly, "W*")
          items = ["M3/BT3"; "Sense Evil"] }]
      page = 64 }
    { name = "Purify Air"
      college = "Air"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI1"] }; { heading = (Druidic, "D")
                               items = ["PI1"] }; { heading = (Wizardly, "W")
                                                    items = ["M0"] }]
      page = 16 }
    { name = "Purify Earth"
      college = "Earth/Plant"
      prereqs =
       [{ heading = (Druidic, "D")
          items = ["PI1"] }; { heading = (Wizardly, "W")
                               items = ["6 Earth spells including Create Earth"] }]
      page = 28 }
    { name = "Purify Food"
      college = "Food"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI2"] }; { heading = (Druidic, "D")
                               items = ["PI2"] }; { heading = (Wizardly, "W")
                                                    items = ["Decay"] }]
      page = 33 }
    { name = "Purify Water"
      college = "Water"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI1"] }; { heading = (Druidic, "D")
                               items = ["PI1"] }; { heading = (Wizardly, "W")
                                                    items = ["Seek Water"] }]
      page = 70 }
    { name = "Quick March"
      college = "Move."
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["M1"; "Haste"] }]
      page = 58 }
    { name = "Recover Energy"
      college = "Healing/Meta"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI1"] }; { heading = (Druidic, "D")
                               items = ["PI1"] }; { heading = (Wizardly, "W")
                                                    items = ["M1"; "Lend Energy"] }]
      page = 38 }
    { name = "Reflect"
      college = "Meta"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Ward"] }]
      page = 52 }
    { name = "Reflect Gaze"
      college = "P&W"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Mirror"] }]
      page = 65 }
    { name = "Reflexes"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Grace"; "Haste"] }]
      page = 22 }
    { name = "Regeneration"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI4"] }]
      page = 38 }
    { name = "Rejoin"
      college = "M&B"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Weaken"; "Restore"] }]
      page = 49 }
    { name = "Relieve Paralysis"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }]
      page = 38 }
    { name = "Relieve Sickness"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }]
      page = 38 }
    { name = "Remember Path"
      college = "Know."
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }]
      page = 44 }
    { name = "Remove Curse"
      college = "Meta"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI5"] }]
      page = 52 }
    { name = "Repair"
      college = "M&B"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M2"; "Rejoin"] }]
      page = 49 }
    { name = "Repel Animal"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }]
      page = 19 }
    { name = "Repel Hybrids"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }]
      page = 19 }
    { name = "Repel Spirits"
      college = "Necro."
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }; { heading = (Wizardly, "W")
                                        items = ["Banish"; "Turn Spirit"] }]
      page = 60 }
    { name = "Resist Acid"
      college = "P&W/Water"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }]
      page = 70 }
    { name = "Resist Cold"
      college = "Fire"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI2"] }; { heading = (Druidic, "D")
                               items = ["PI3"] }; { heading = (Wizardly, "W")
                                                    items = ["Heat"] }]
      page = 31 }
    { name = "Resist Disease"
      college = "P&W/Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }]
      page = 38 }
    { name = "Resist Fire"
      college = "Fire"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["Fireproof"] }]
      page = 31 }
    { name = "Resist Lightning"
      college = "Air/P&W/Weather"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI2"] }; { heading = (Druidic, "D")
                               items = ["PI3"] }; { heading = (Wizardly, "W")
                                                    items = ["6 Air spells"] }]
      page = 72 }
    { name = "Resist Pain"
      college = "Body"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["M2"; "Pain"] }]
      page = 22 }
    { name = "Resist Poison"
      college = "Healing/P&W"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }]
      page = 38 }
    { name = "Resist Sound"
      college = "P&W/Sound"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["4 Sound spells"] }]
      page = 67 }
    { name = "Resist Water"
      college = "P&W/Water"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Umbrella"; "or Shape Water"; "Destroy Water"] }]
      page = 70 }
    { name = "Restoration"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }]
      page = 38 }
    { name = "Restore"
      college = "M&B"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Find Weakness or Simple Illusion"] }]
      page = 49 }
    { name = "Restore Hearing"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }]
      page = 39 }
    { name = "Restore Memory"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }]
      page = 39 }
    { name = "Restore Sight"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }]
      page = 39 }
    { name = "Restore Speech"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }]
      page = 39 }
    { name = "Retch"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Nauseate"; "Spasm"] }]
      page = 22 }
    { name = "Rider"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }]
      page = 19 }
    { name = "Rider Within"
      college = "Animal"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }]
      page = 20 }
    { name = "Rive"
      college = "M&B"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M2"; "Shatter"] }]
      page = 49 }
    { name = "Rooted Feet"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Hinder"] }]
      page = 22 }
    { name = "Roundabout"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Tanglefoot"] }]
      page = 22 }
    { name = "Sanctuary"
      college = "Gate"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI6"] }]
      page = 35 }
    { name = "Sandstorm"
      college = "Air/Earth"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI4"] }; { heading = (Wizardly, "W")
                                        items = ["Create Earth"; "Windstorm"] }]
      page = 16 }
    { name = "Scry Gate"
      college = "Gate"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Seek Gate"] }]
      page = 35 }
    { name = "Scryguard"
      college = "Meta"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"] }]
      page = 52 }
    { name = "See Invisible"
      college = "L&D"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Invisibility"; "or Dark Vision"; "Infravision"] }]
      page = 48 }
    { name = "See Secrets"
      college = "Know."
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }; { heading = (Wizardly, "W*")
                                        items = ["Seeker"; "Aura"] }]
      page = 44 }
    { name = "Seek Earth"
      college = "Earth"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["M0"] }]
      page = 28 }
    { name = "Seek Fire"
      college = "Fire"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M0"] }]
      page = 31 }
    { name = "Seek Food"
      college = "Food"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["M0"] }]
      page = 33 }
    { name = "Seek Gate"
      college = "Gate"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M2"; "Seek Magic"; "1 spell from 10 colleges"] }]
      page = 35 }
    { name = "Seek Magic"
      college = "Know./Meta"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Detect Magic"] }]
      page = 45 }
    { name = "Seek Plant"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI1"] }]
      page = 62 }
    { name = "Seek Water"
      college = "Water"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["M0"] }]
      page = 70 }
    { name = "Seeker"
      college = "Know."
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI2"] }; { heading = (Wizardly, "W*")
                               items = ["M1/BT1"; "IQ 12+"; "2 “Seek” spells"] }]
      page = 45 }
    { name = "Sense Danger"
      college = "P&W"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Sense Foes"] }]
      page = 65 }
    { name = "Sense Emotion"
      college = "C&E"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Sense Foes"] }]
      page = 26 }
    { name = "Sense Evil"
      college = "C&E/Meta"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI1"] }; { heading = (Druidic, "D")
                               items = ["PI1"] }; { heading = (Wizardly, "W*")
                                                    items = ["M2/BT2"] }]
      page = 26 }
    { name = "Sense Foes"
      college = "C&E"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M0/BT1"] }]
      page = 26 }
    { name = "Sense Life"
      college = "C&E"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI1"] }; { heading = (Druidic, "D")
                               items = ["PI1"] }; { heading = (Wizardly, "W*")
                                                    items = ["M0/BT1"] }]
      page = 26 }
    { name = "Sense Spirit"
      college = "Necro."
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI1"] }; { heading = (Wizardly, "W")
                               items = ["Death Vision"; "or M1"; "Sense Life"] }]
      page = 60 }
    { name = "Sensitize"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"; "Stun"] }]
      page = 22 }
    { name = "Shape Air"
      college = "Air"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["Create Air"] }]
      page = 17 }
    { name = "Shape Earth"
      college = "Earth"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["Seek Earth"] }]
      page = 28 }
    { name = "Shape Fire"
      college = "Fire"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Ignite Fire"] }]
      page = 31 }
    { name = "Shape Plant"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }]
      page = 63 }
    { name = "Shape Water"
      college = "Water"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["Create Water"] }]
      page = 70 }
    { name = "Share Energy"
      college = "Healing/Meta"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI1"] }; { heading = (Druidic, "D")
                               items = ["PI1"] }; { heading = (Wizardly, "W")
                                                    items = ["Lend Energy"] }]
      page = 39 }
    { name = "Share Vitality"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }]
      page = 39 }
    { name = "Sharpen"
      college = "M&B"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Repair"] }]
      page = 50 }
    { name = "Shatter"
      college = "M&B"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"; "Weaken"] }]
      page = 50 }
    { name = "Shatterproof"
      college = "M&B"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Repair"; "Shatter"] }]
      page = 50 }
    { name = "Shield"
      college = "P&W"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["M2"] }]
      page = 65 }
    { name = "Shocking Touch"
      college = "Air/Weather"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Lightning"] }]
      page = 72 }
    { name = "Sickness"
      college = "Body/Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Drunkenness"] }]
      page = 55 }
    { name = "Silence"
      college = "Sound"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }; { heading = (Wizardly, "W*")
                                        items = ["Sound"] }]
      page = 67 }
    { name = "Silver Tongue"
      college = "Sound"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI3"] }; { heading = (Wizardly, "W*")
                               items = ["Voices"; "5 Mind Control spells"] }]
      page = 67 }
    { name = "Simple Illusion"
      college = "Illusion"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M0"; "IQ 11+"; "no Blindness"] }]
      page = 42 }
    { name = "Sleep"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Daze"] }]
      page = 55 }
    { name = "Slow"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"; "Haste"; "Hinder"] }]
      page = 58 }
    { name = "Slow Fall"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Apportation"] }]
      page = 58 }
    { name = "Smoke"
      college = "Fire"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Shape Fire"; "Extinguish Fire"] }]
      page = 32 }
    { name = "Soul Rider"
      college = "C&E"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Mind-Reading"] }]
      page = 26 }
    { name = "Sound"
      college = "Sound"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M0/BT1"] }]
      page = 67 }
    { name = "Sound Jet"
      college = "Sound"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Great Voice"] }]
      page = 67 }
    { name = "Spark Cloud"
      college = "Air/Weather"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Lightning"; "Shape Air"] }]
      page = 72 }
    { name = "Spark Storm"
      college = "Air/Weather"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI5"] }; { heading = (Wizardly, "W")
                                        items = ["Lightning"; "Windstorm"] }]
      page = 72 }
    { name = "Spasm"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Itch"] }]
      page = 22 }
    { name = "Spell Shield"
      college = "Meta"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M2"; "Magic Resistance"; "Scryguard"] }]
      page = 52 }
    { name = "Steelwraith"
      college = "Earth"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M2"; "Walk Through Earth"] }]
      page = 28 }
    { name = "Stench"
      college = "Air"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Purify Air"] }]
      page = 17 }
    { name = "Stiffen"
      college = "M&B"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Rejoin"] }]
      page = 50 }
    { name = "Stone Missile"
      college = "Earth"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Create Earth"] }]
      page = 28 }
    { name = "Stone to Earth"
      college = "Earth"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Earth to Stone or 4 Earth spells"] }]
      page = 28 }
    { name = "Stone to Flesh"
      college = "Earth"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI3"] }; { heading = (Wizardly, "W")
                               items = ["M2"; "Flesh to Stone"; "Stone to Earth"] }]
      page = 29 }
    { name = "Stop Bleeding"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }]
      page = 39 }
    { name = "Stop Paralysis"
      college = "Body/Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }]
      page = 40 }
    { name = "Stop Spasm"
      college = "Body/Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }]
      page = 40 }
    { name = "Strengthen Will"
      college = "Mind"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI3"] }; { heading = (Wizardly, "W*")
                               items = ["M1/BT1"; "6 Mind Control spells"] }]
      page = 55 }
    { name = "Strike Blind"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Spasm"; "2 L&D spells"] }]
      page = 22 }
    { name = "Strike Deaf"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Spasm"; "2 Sound spells"] }]
      page = 22 }
    { name = "Strike Dumb"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Spasm"] }]
      page = 23 }
    { name = "Stun"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Pain"] }]
      page = 23 }
    { name = "Summon Spirit"
      college = "Necro."
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["M2"; "Death Vision"] }]
      page = 60 }
    { name = "Sunbolt"
      college = "L&D"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI3"] }; { heading = (Wizardly, "W")
                               items = ["6 L&D spells including Sunlight"] }]
      page = 48 }
    { name = "Sunlight"
      college = "L&D"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI3"] }; { heading = (Druidic, "D")
                               items = ["PI3"] };
        { heading = (Wizardly, "W")
          items = ["M1"; "Colors"; "Glow"] }]
      page = 48 }
    { name = "Suspended Animation"
      college = "Healing"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }]
      page = 40 }
    { name = "Swim"
      college = "Move./Water"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }; { heading = (Wizardly, "W")
                                        items = ["Levitation"; "Shape Water"] }]
      page = 70 }
    { name = "Tangle Growth"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }]
      page = 63 }
    { name = "Tanglefoot"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Clumsiness"] }]
      page = 23 }
    { name = "Telepathy"
      college = "C&E"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Mind-Sending"] }]
      page = 26 }
    { name = "Teleport Shield"
      college = "Gate/P&W"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Spell Shield"; "Watchdog"] }]
      page = 65 }
    { name = "Tell Position"
      college = "Know."
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI1"] }; { heading = (Wizardly, "W*")
                                        items = ["Measurement"] }]
      page = 45 }
    { name = "Terror"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Fear"] }]
      page = 55 }
    { name = "Test Food"
      college = "Food"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["M0"] }]
      page = 33 }
    { name = "Test Load"
      college = "Know."
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Measurement"] }]
      page = 45 }
    { name = "Thunderclap"
      college = "Sound"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI1"] }; { heading = (Druidic, "D")
                               items = ["PI1"] }; { heading = (Wizardly, "W*")
                                                    items = ["Sound"] }]
      page = 67 }
    { name = "Tickle"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Spasm"] }]
      page = 23 }
    { name = "Total Paralysis"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Paralyze Limb"] }]
      page = 23 }
    { name = "Trace"
      college = "Know."
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Seeker"] }]
      page = 45 }
    { name = "Trace Teleport"
      college = "Gate/Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M2"; "IQ 13+"; "1 spell from 10 colleges"] }]
      page = 35 }
    { name = "Truthsayer"
      college = "C&E"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }; { heading = (Wizardly, "W*")
                                        items = ["Sense Emotion"] }]
      page = 26 }
    { name = "Turn Spirit"
      college = "Necro."
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["Fear"; "Sense Spirit"] }]
      page = 61 }
    { name = "Turn Zombie"
      college = "Necro."
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI2"] }]
      page = 61 }
    { name = "Umbrella"
      college = "P&W/Water"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI1"] }; { heading = (Druidic, "D")
                               items = ["PI1"] };
        { heading = (Wizardly, "W")
          items = ["Shape Water or Shield"] }]
      page = 70 }
    { name = "Undo"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Locksmith"] }]
      page = 59 }
    { name = "Vigil"
      college = "Mind"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI4"] }]
      page = 56 }
    { name = "Vigor"
      college = "Body"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["Frailty or Might"] }]
      page = 23 }
    { name = "Voices"
      college = "Sound"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Sound"] }]
      page = 68 }
    { name = "Walk on Air"
      college = "Air"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Shape Air"] }]
      page = 17 }
    { name = "Walk on Water"
      college = "Water"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Shape Water"] }]
      page = 71 }
    { name = "Walk Through Earth"
      college = "Earth"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["4 Earth spells"] }]
      page = 29 }
    { name = "Walk Through Plants"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }]
      page = 63 }
    { name = "Walk Through Wood"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }]
      page = 63 }
    { name = "Wall of Lightning"
      college = "Air/Weather"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Lightning"] }]
      page = 72 }
    { name = "Wall of Silence"
      college = "Sound"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Silence"] }]
      page = 68 }
    { name = "Wallwalker"
      college = "Move."
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Apportation"] }]
      page = 59 }
    { name = "Ward"
      college = "Meta"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M1"] }]
      page = 53 }
    { name = "Warmth"
      college = "Fire/P&W"
      prereqs =
       [{ heading = (Clerical, "C")
          items = ["PI1"] }; { heading = (Druidic, "D")
                               items = ["PI1"] }; { heading = (Wizardly, "W")
                                                    items = ["Heat"] }]
      page = 32 }
    { name = "Watchdog"
      college = "P&W"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI1"] }; { heading = (Wizardly, "W")
                                        items = ["Sense Danger"] }]
      page = 65 }
    { name = "Water Jet"
      college = "Water"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Shape Water"] }]
      page = 71 }
    { name = "Water Vision"
      college = "Know./Water"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI3"] }; { heading = (Wizardly, "W*")
                                        items = ["Shape Water"] }]
      page = 71 }
    { name = "Weaken"
      college = "M&B"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["Find Weakness"] }]
      page = 50 }
    { name = "Weaken Will"
      college = "Mind"
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["M1/BT1"; "Foolishness"] }]
      page = 56 }
    { name = "Weather Dome"
      college = "P&W/Weather"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }]
      page = 65 }
    { name = "Windstorm"
      college = "Air"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI2"] }; { heading = (Wizardly, "W")
                                        items = ["Shape Air"] }]
      page = 17 }
    { name = "Wisdom"
      college = "Mind"
      prereqs = [{ heading = (Clerical, "C")
                   items = ["PI3"] }; { heading = (Wizardly, "W*")
                                        items = ["6 Mind Control spells"] }]
      page = 56 }
    { name = "Wither Limb"
      college = "Body"
      prereqs = [{ heading = (Wizardly, "W")
                   items = ["M2"; "Paralyze Limb"] }]
      page = 23 }
    { name = "Wither Plant"
      college = "Plant"
      prereqs = [{ heading = (Druidic, "D")
                   items = ["PI4"] }]
      page = 63 }
    { name = "Wizard Eye"
      college = "Know."
      prereqs = [{ heading = (Wizardly, "W*")
                   items = ["Apportation"; "Keen Vision"] }]
      page = 45 }
    ]

spells |> List.map(fun s -> s.college) |> List.distinct |> List.sort