/**
 * Extend class using object : split methode on multiple files + bind
 * @note taken from https://stackoverflow.com/a/61860802/1934482
 * @example class Test extends Classes([a,b]){ constructor(){super(), ...}}
 * @param {Array} sub classes to add
 * @return {Class}
 */
export function Classes(bases) {
  class Bases {
    constructor() {
      for (const base of bases) {
        Object.assign(this, new base());
      }
    }
  }

  for (const base of bases) {
    Object.getOwnPropertyNames(base.prototype)
      .filter((prop) => prop !== "constructor")
      .forEach((prop) => (Bases.prototype[prop] = base.prototype[prop]));
  }

  return Bases;
}
