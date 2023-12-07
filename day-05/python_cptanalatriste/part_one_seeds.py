from typing import Optional, TypedDict
from tqdm import tqdm


class MappingRange(TypedDict):
    source_start: int
    source_end: int
    destination_start: int
    destination_end: int


class Mapper:
    def __init__(self, lines: list[str]):
        # print(f"Parsing mapper with {len(lines)} lines")
        self.ranges: list[MappingRange] = []

        # The first line has a destination range start of 50,
        #  a source range start of 98, and a range length of 2.

        for line in lines:
            destination_string, source_string, range_string = line.split(" ")
            destination_start: int = int(destination_string)
            source_start: int = int(source_string)
            range_length: int = int(range_string)

            self.ranges.append(
                MappingRange(
                    source_start=source_start,
                    source_end=source_start + range_length,
                    destination_start=destination_start,
                    destination_end=destination_start + range_length,
                )
            )

        self.ranges = sorted(
            self.ranges, key=lambda range: range["source_start"]
        )

    def get_destination_from_source(self, source: int) -> int:
        for mapping_range in self.ranges:
            if (
                mapping_range["source_start"]
                <= source
                < mapping_range["source_end"]
            ):
                destination: int = mapping_range["destination_start"] + (
                    source - mapping_range["source_start"]
                )
                return destination

        return source

    def get_source_from_destination(self, destination: int) -> int:
        for mapping_range in self.ranges:
            if (
                mapping_range["destination_start"]
                <= destination
                < mapping_range["destination_end"]
            ):
                source: int = mapping_range["source_start"] + (
                    destination - mapping_range["destination_start"]
                )
                return source

        return destination


class SeedAnaliser:
    def __init__(
        self,
        seed_to_soil: Mapper,
        soil_to_fertiliser: Mapper,
        fertiliser_to_water: Mapper,
        water_to_light: Mapper,
        light_to_temperature: Mapper,
        temperature_to_humidity: Mapper,
        humidity_to_location: Mapper,
    ):
        self.seed_to_soil: Mapper = seed_to_soil
        self.soil_to_fertiliser: Mapper = soil_to_fertiliser
        self.fertiliser_to_water: Mapper = fertiliser_to_water
        self.water_to_light: Mapper = water_to_light
        self.light_to_temperature: Mapper = light_to_temperature
        self.temperature_to_humidity: Mapper = temperature_to_humidity
        self.humidity_to_location: Mapper = humidity_to_location

    def analyse_seed(self, seed: int) -> dict[str, int]:
        soil: int = self.seed_to_soil.get_destination_from_source(seed)
        fertiliser: int = self.soil_to_fertiliser.get_destination_from_source(
            soil
        )
        water: int = self.fertiliser_to_water.get_destination_from_source(
            fertiliser
        )
        light: int = self.water_to_light.get_destination_from_source(water)
        temperature: int = (
            self.light_to_temperature.get_destination_from_source(light)
        )
        humidity: int = (
            self.temperature_to_humidity.get_destination_from_source(
                temperature
            )
        )
        location: int = self.humidity_to_location.get_destination_from_source(
            humidity
        )

        return {
            "soil": soil,
            "fertiliser": fertiliser,
            "water": water,
            "light": light,
            "temperature": temperature,
            "humidity": humidity,
            "location": location,
        }

    def analyse_location(self, location: int) -> dict[str, int]:
        humidity: int = self.humidity_to_location.get_source_from_destination(
            location
        )

        temperature: int = (
            self.temperature_to_humidity.get_source_from_destination(humidity)
        )

        light: int = self.light_to_temperature.get_source_from_destination(
            temperature
        )

        water: int = self.water_to_light.get_source_from_destination(light)

        fertiliser: int = self.fertiliser_to_water.get_source_from_destination(
            water
        )

        soil: int = self.soil_to_fertiliser.get_source_from_destination(
            fertiliser
        )

        seed: int = self.seed_to_soil.get_source_from_destination(soil)

        return {
            "seed": seed,
            "soil": soil,
            "fertiliser": fertiliser,
            "water": water,
            "light": light,
            "humidity": humidity,
            "temperature": temperature,
        }


def process_input_file(file_name: str) -> tuple[SeedAnaliser, list[int]]:
    seeds: list[int] = []
    seed_to_soil_lines: list[str] = []
    soil_to_fertiliser_lines: list[str] = []
    fertiliser_to_water_lines: list[str] = []
    water_to_light_lines: list[str] = []
    light_to_temperature_lines: list[str] = []
    temperature_to_humidity_lines: list[str] = []
    humidity_to_location_lines: list[str] = []

    current_list: list[str] = []
    with open(file_name) as input_file:
        for line in input_file:
            # print(f"{line=}")
            line = line.rstrip("\n")
            if line.startswith("seeds:"):
                seed_strings: str = line.split(":")[1]
                seeds = [
                    int(seed_string)
                    for seed_string in seed_strings.split(" ")
                    if seed_string != ""
                ]
            elif line.startswith("seed-to-soil map:"):
                current_list = seed_to_soil_lines
            elif line.startswith("soil-to-fertilizer map:"):
                current_list = soil_to_fertiliser_lines
            elif line.startswith("fertilizer-to-water map:"):
                current_list = fertiliser_to_water_lines
            elif line.startswith("water-to-light map:"):
                current_list = water_to_light_lines
            elif line.startswith("light-to-temperature map:"):
                current_list = light_to_temperature_lines
            elif line.startswith("temperature-to-humidity map:"):
                current_list = temperature_to_humidity_lines
            elif line.startswith("humidity-to-location map:"):
                current_list = humidity_to_location_lines
            elif line != "":
                current_list.append(line)

    print("Creating analyser")
    analyser: SeedAnaliser = SeedAnaliser(
        seed_to_soil=Mapper(seed_to_soil_lines),
        soil_to_fertiliser=Mapper(soil_to_fertiliser_lines),
        fertiliser_to_water=Mapper(fertiliser_to_water_lines),
        water_to_light=Mapper(water_to_light_lines),
        light_to_temperature=Mapper(light_to_temperature_lines),
        temperature_to_humidity=Mapper(temperature_to_humidity_lines),
        humidity_to_location=Mapper(humidity_to_location_lines),
    )

    print("Analyser created")

    return analyser, seeds


def main(file_name: str) -> int:
    print("Starting parsing...")
    analyser, seeds = process_input_file(file_name)
    print("Parsing ended...")

    answer: Optional[int] = None

    for seed in tqdm(seeds):
        seed_location: int = analyser.analyse_seed(seed)["location"]
        # print(f"Finish with {seed=}. {seed_location=}")

        if answer is None or seed_location < answer:
            answer = seed_location

    if answer is not None:
        return answer

    return -1


if __name__ == "__main__":
    print("Starting...")
    print(main("python_cptanalatriste/input.txt"))
    print("Finished!")
