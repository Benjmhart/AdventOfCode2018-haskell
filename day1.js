import input from './inputs/day1.json';

//part 1
// const results = input.reduce((a, x) => a + x, 0);
// results; //?

const example = [3, 3, 4, -2, -4]; //?
// part 2
const getResults = acc =>
  input.reduce((as, x) => {
    if (typeof as === 'number') {
      return as;
    }
    const last = as[as.length - 1];
    const next = Number(last) + Number(x);

    if (as.includes(next)) {
      return next;
    }
    return as.concat([next]);
  }, acc);

const getResults2 = start => {
  const thisResult = getResults(start);
  console.log(start);
  if (typeof thisResult === 'number') {
    return thisResult;
  }
  return getResults2(thisResult);
};

const results2 = getResults2([0]); //?
