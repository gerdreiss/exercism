pub struct Player {
    pub health: u32,
    pub mana: Option<u32>,
    pub level: u32,
}

impl Player {
    pub fn revive(&self) -> Option<Player> {
        if self.health > 0 {
            None
        } else {
            Some(Player {
                health: 100,
                mana: if self.level >= 10 { Some(100) } else { None },
                level: self.level,
            })
        }
    }

    pub fn cast_spell(&mut self, mana_cost: u32) -> u32 {
        if let Some(mana) = self.mana {
            if mana < mana_cost {
                0
            } else {
                self.mana = self.mana.map(|m| m - mana_cost);
                mana_cost * 2
            }
        } else {
            self.health = self.health.checked_sub(mana_cost).unwrap_or_default();
            0
        }
    }
}
