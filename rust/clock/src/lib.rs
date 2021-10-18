use std::fmt::{Display, Formatter};
use std::ops::Add;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Clock {
    hours: i32,
    minutes: i32,
}

impl Add<Clock> for Clock {
    type Output = Clock;

    fn add(self, rhs: Clock) -> Self::Output {
        let minutes_sum = self.minutes + rhs.minutes;
        let hours_sum = self.hours + rhs.hours + minutes_sum / 60;
        Self::Output {
            hours: hours_sum % 24,
            minutes: minutes_sum % 60,
        }
    }
}

impl Display for Clock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:0>2}:{:0>2}", self.hours, self.minutes)
    }
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        if hours < 0 {
            Clock::new(24 + hours, minutes)
        } else if minutes < 0 {
            Clock::new(hours - 1, 60 + minutes)
        } else {
            Self {
                hours: (hours + minutes / 60) % 24,
                minutes: minutes % 60,
            }
        }
    }
    pub fn add_minutes(&self, minutes: i32) -> Self {
        *self + Clock::new(minutes / 60, minutes % 60)
    }
}
