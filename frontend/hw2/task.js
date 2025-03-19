function makePotionsRoom() {
  class my_storage {
    constructor() {
      this.store = new Map();
    }
    add(shelveName, potion) {
      if (!this.store.has(shelveName)) {
        this.store.set(shelveName, [potion])
      } else {
        this.store.get(shelveName).push(potion);
      }
    }

    takePotion(potion) {
      for (let [key, value] of this.store) {
        let index = value.findIndex((p) => p.name === potion);
        if (index !== -1) {
          this.store.get(key).splice(index, 1);
          return potion;
        }
      }
      return null;
    }

    getAllPotionsFromShelve(shelveName) {
      const byShelveName = this.store.get(shelveName);
      return byShelveName;
    }

    getAllPotions() {
      let out = [];
      for (let [key, potions] of this.store) {
        out = out.concat(potions);
      }
      return out;
    }

    takeAllPotionsFromShelve(shelveName) {
      let out = this.store.get(shelveName);
      this.store.set(shelveName, []);
      return out
    }

    usePotion(potionName) {
      for (let [key, value] of this.store) {
        let index = value.findIndex((p) => p.name == potionName);
        if (index !== -1) {
          let potion = value[index];
          potion.use();
          this.store.get(key).splice(index, 1);
        }
      }
    }

    useAllPotionsFromShelve(shelveName) {
      let = this.store.get(shelveName);
      let potionsToUse = [...array];
      for (let value of potionsToUse) {
        value.use();
      }
      this.store.set(shelveName, []);
    }

    clean(revisionDay) {
      let out = [];
      let predicate = (potion) => {
        let curDate = new Date(potion.created);
        curDate.setDate(curDate.getDate() + potion.expirationDays);
        return curDate < revisionDay;
      };
      for (let [key, array] of this.store) {
        let fil_elems = array.filter(predicate);
        out.push(...fil_elems);
        this.store.set(key, array.filter(potion => !predicate(potion)));
      }
      return out;
    }


    uniquePotionsCount() {
      let set = new Set();
      for (let [key, array] of this.store) {
        for (let value of array) {
          set.add(value.name);
        }
      }
      return set.size;
    }
  }
  const storage = new my_storage();
  return storage;
}

