package tests

var responses = map[string][]byte{
	"get-controller": []byte{
		0x17, 0x94, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xc0, 0xa8, 0x01, 0x64, 0xff, 0xff, 0xff, 0x00,
		0xc0, 0xa8, 0x01, 0x01, 0x00, 0x12, 0x23, 0x34, 0x45, 0x56, 0x08, 0x92, 0x20, 0x18, 0x11, 0x05,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},

	"get-status": []byte{
		0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xe3, 0x01, 0x00, 0x00, 0x01, 0x01, 0x03, 0x01,
		0xa0, 0x7a, 0x99, 0x00, 0x20, 0x23, 0x10, 0x05, 0x12, 0x23, 0x38, 0x12, 0x01, 0x00, 0x00, 0x01,
		0x00, 0x01, 0x01, 0x00, 0x56, 0x09, 0x02, 0x48, 0x94, 0x26, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0xfd, 0x12, 0x34, 0x23, 0x10, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},

	"get-status-no-event": []byte{
		0x17, 0x20, 0x00, 0x00, 0x41, 0x78, 0x1e, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x01,
		0x00, 0x01, 0x01, 0x00, 0x56, 0x09, 0x02, 0x48, 0x94, 0x26, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0xfd, 0x12, 0x34, 0x23, 0x10, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}
