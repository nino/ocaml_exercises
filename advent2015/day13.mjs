import { readFile } from "node:fs/promises";

function* permutations(items) {
	if (items.size === 0) {
		yield [];
		return;
	}

	for (const item of items) {
		const others = new Set(items);
		others.delete(item);
		for (const perm of permutations(others)) {
			yield [item, ...perm];
		}
	}
}

const input = (await readFile("assets/day13input.txt")).toString();
const lines = input.split("\n");

const happinesses = new Map();
const people = new Set();

const parseLine = (line) => {
	const regex =
		/(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+)\./;
	const matches = line.match(regex);
	if (matches == null) {
		return;
	}
	const [_, name1, direction, numPoints, name2] = matches;
	people.add(name1);
	people.add(name2);
	const pointDiff =
		direction === "gain" ? Number(numPoints) : -Number(numPoints);
	happinesses.set([name1, name2].join(","), pointDiff);
};

lines.forEach(parseLine);
console.log({ happinesses });

const mod = (a, b) => ((a % b) + b) % b;

const neighbors = (perm, idx) => [
	perm[mod(idx - 1, perm.length)],
	perm[mod(idx + 1, perm.length)],
];

const happinessForPermutation = (perm) => {
	let happiness = 0;
	perm.forEach((person, idx) => {
		const [n1, n2] = neighbors(perm, idx);
		happiness += happinesses.get([person, n1].join(",")) ?? 0;
		happiness += happinesses.get([person, n2].join(",")) ?? 0;
		console.log(
			`${person} sits next to ${n1} and ${n2} now, and the happiness is ${happiness}.`,
		);
	});
	return happiness;
};

let bestChangeInHappiness = -Infinity;

for (const perm of permutations(people)) {
	const happiness = happinessForPermutation(perm);
	// console.log("Happiness for perm", perm, "is", happiness);
	if (happiness > bestChangeInHappiness) {
		bestChangeInHappiness = happiness;
	}
}

console.log(
	"For the optimal arrangement, the change in happiness is",
	bestChangeInHappiness,
);
