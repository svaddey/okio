package com.braviithi.okio.service;

import java.io.File;

public interface IS3Service {
	public void downloadFile(String keyName);
	public String uploadFile(String keyName, File file);
	public void modifyACL(String keyName);
}
