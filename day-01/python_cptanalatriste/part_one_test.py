import calibration


def test_calibration() -> None:
    assert 12 == calibration.get_calibration_value("1abc2")
    assert 38 == calibration.get_calibration_value("pqr3stu8vwx")
    assert 15 == calibration.get_calibration_value("a1b2c3d4e5f")
    assert 77 == calibration.get_calibration_value("treb7uchet")
