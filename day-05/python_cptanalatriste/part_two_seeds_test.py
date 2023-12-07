from python_cptanalatriste import (
    part_two_seeds,
    part_one_seeds,
    part_one_seeds_test,
)


def test_seed_ranges() -> None:
    seed_to_soil: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.seed_to_soil_lines
    )
    soil_to_fertiliser: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.soil_to_fertiliser_lines
    )
    fertiliser_to_water: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.fertiliser_to_water_lines
    )
    water_to_light: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.water_to_light_lines
    )
    light_to_temperature: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.light_to_temperature_lines
    )
    temperature_to_humidity: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.temperature_to_humidity_lines
    )
    humidity_to_location: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.humidity_to_location_lines
    )

    analyser: part_one_seeds.SeedAnaliser = part_one_seeds.SeedAnaliser(
        seed_to_soil=seed_to_soil,
        soil_to_fertiliser=soil_to_fertiliser,
        fertiliser_to_water=fertiliser_to_water,
        water_to_light=water_to_light,
        light_to_temperature=light_to_temperature,
        temperature_to_humidity=temperature_to_humidity,
        humidity_to_location=humidity_to_location,
    )

    seed_ranges: list[int] = [79, 14, 55, 13]

    assert 46 == part_two_seeds.process_seed_ranges(seed_ranges, analyser)


def test_seed_ranges_reverse() -> None:
    seed_to_soil: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.seed_to_soil_lines
    )
    soil_to_fertiliser: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.soil_to_fertiliser_lines
    )
    fertiliser_to_water: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.fertiliser_to_water_lines
    )
    water_to_light: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.water_to_light_lines
    )
    light_to_temperature: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.light_to_temperature_lines
    )
    temperature_to_humidity: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.temperature_to_humidity_lines
    )
    humidity_to_location: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.humidity_to_location_lines
    )

    analyser: part_one_seeds.SeedAnaliser = part_one_seeds.SeedAnaliser(
        seed_to_soil=seed_to_soil,
        soil_to_fertiliser=soil_to_fertiliser,
        fertiliser_to_water=fertiliser_to_water,
        water_to_light=water_to_light,
        light_to_temperature=light_to_temperature,
        temperature_to_humidity=temperature_to_humidity,
        humidity_to_location=humidity_to_location,
    )

    seed_ranges: list[int] = [79, 14, 55, 13]

    assert 46 == part_two_seeds.process_in_reverse(seed_ranges, analyser)


def test_analyse_location() -> None:
    seed_to_soil: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.seed_to_soil_lines
    )
    soil_to_fertiliser: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.soil_to_fertiliser_lines
    )
    fertiliser_to_water: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.fertiliser_to_water_lines
    )
    water_to_light: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.water_to_light_lines
    )
    light_to_temperature: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.light_to_temperature_lines
    )
    temperature_to_humidity: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.temperature_to_humidity_lines
    )
    humidity_to_location: part_one_seeds.Mapper = part_one_seeds.Mapper(
        part_one_seeds_test.humidity_to_location_lines
    )

    analyser: part_one_seeds.SeedAnaliser = part_one_seeds.SeedAnaliser(
        seed_to_soil=seed_to_soil,
        soil_to_fertiliser=soil_to_fertiliser,
        fertiliser_to_water=fertiliser_to_water,
        water_to_light=water_to_light,
        light_to_temperature=light_to_temperature,
        temperature_to_humidity=temperature_to_humidity,
        humidity_to_location=humidity_to_location,
    )

    location_number: int = 82
    assert analyser.analyse_location(location_number) == {
        "seed": 79,
        "soil": 81,
        "fertiliser": 81,
        "water": 81,
        "light": 74,
        "temperature": 78,
        "humidity": 78,
    }
