type Input = string;
type Packet = {
  version: number;
  typeId: number;
  literalValue?: number;
  length: number;
  packets?: Packet[];
};

function hex2bin(hex: string): string {
  return hex
    .split('')
    .map(h => parseInt(h, 16).toString(2).padStart(4, '0'))
    .join('');
}

function parsePacket(bits: string): Packet {
  let version = parseInt(bits.slice(0, 3), 2);
  let typeId = parseInt(bits.slice(3, 6), 2);
  if (typeId === 4) {
    let groups = bits.slice(6);
    let literalValueBin = '';
    let groupsLength = 0; // Length of the 5-bit groups only

    for (let i = 0; i < groups.length; i += 5) {
      literalValueBin += groups.slice(i + 1, i + 5);
      groupsLength += 5;
      if (groups[i] === '0') break;
    }
    return {
      version,
      typeId,
      literalValue: parseInt(literalValueBin, 2),
      length: 6 + groupsLength,
    };
  } else {
    let lengthTypeId = bits[6];
    let packets: Packet[] = [];
    if (lengthTypeId === '0') {
      // Next 15 is length
      let subPacketsLength = parseInt(bits.slice(7, 22), 2);
      let subPacketsBits = bits.slice(22, 22 + subPacketsLength);
      let parsedLength = 0;
      while (parsedLength < subPacketsLength) {
        let subPacket = parsePacket(subPacketsBits.slice(parsedLength));
        packets.push(subPacket);
        parsedLength += subPacket.length!;
      }
      return {
        version,
        typeId,
        packets,
        length: 22 + subPacketsLength,
      };
    } else {
      // lengthTypeId === '1'
      // Next 11 is sub-packet count
      let subPacketsCount = parseInt(bits.slice(7, 18), 2);
      let bitsRead = 0;
      let subPacketsBits = bits.slice(18);
      for (let i = 0; i < subPacketsCount; i++) {
        let packet = parsePacket(subPacketsBits.slice(bitsRead));
        packets.push(packet);
        bitsRead += packet.length;
      }
      return {
        version,
        typeId,
        packets,
        length: 18 + bitsRead,
      };
    }
  }
}

function sumVersions(packet: Packet): number {
  let sum = packet?.packets?.reduce((sum, p) => sum + sumVersions(p), 0) ?? 0;
  return packet.version + sum;
}

function evalPacket(packet: Packet): number {
  switch (packet.typeId) {
    case 0:
      return packet.packets!.map(evalPacket).reduce((acc, v) => acc + v);
    case 1:
      return packet.packets!.map(evalPacket).reduce((acc, v) => acc * v);
    case 2:
      return Math.min(...packet.packets!.map(evalPacket));
    case 3:
      return Math.max(...packet.packets!.map(evalPacket));
    case 4:
      return packet.literalValue!;
    case 5:
      return evalPacket(packet.packets![0]) > evalPacket(packet.packets![1]) ? 1 : 0;
    case 6:
      return evalPacket(packet.packets![0]) < evalPacket(packet.packets![1]) ? 1 : 0;
    case 7:
      return evalPacket(packet.packets![0]) === evalPacket(packet.packets![1]) ? 1 : 0;
    default:
      throw new Error(`Unknown type id: ${packet.typeId}`);
  }
}

export function part1(input: Input) {
  let bits = hex2bin(input);
  let packet = parsePacket(bits);

  return sumVersions(packet);
}

export function part2(input: Input) {
  let bits = hex2bin(input);
  let packet = parsePacket(bits);

  return evalPacket(packet);
}

function parseInput(inputString: string): Input {
  return inputString.trim();
}

async function main(useRealInput = true) {
  let inputString = '';
  try {
    inputString =
      // @ts-expect-error: next-line
      typeof Bun !== 'undefined'
        ? // @ts-expect-error: next-line
          await Bun.file(`${import.meta.dir}/input.txt`).text()
        : // @ts-expect-error: next-line
        typeof Deno !== 'undefined'
        ? // @ts-expect-error: next-line
          await Deno.readTextFile(`${import.meta.dirname}/input.txt`)
        : '';
  } catch (error) {
    useRealInput = false;
  }

  let testInput = `9C0141080250320F1802104A08`;

  if (!useRealInput) inputString = testInput;

  console.log('part1:', part1(parseInput(inputString)));
  console.log('part2:', part2(parseInput(inputString)));
}

// await main(false);
await main();
