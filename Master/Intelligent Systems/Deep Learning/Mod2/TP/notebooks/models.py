# models.py
import torch
import torch.nn as nn
from torch.nn import Sequential, Conv2d, ReLU, MaxPool2d, Linear

class CNNModel_1(nn.Module):
    def __init__(self, num_classes=4):
        super(CNNModel_1, self).__init__()
        self.layer1 = Sequential(
            Conv2d(3, 32, 3),
            ReLU(),
            MaxPool2d(2, 2)
        )
        self.layer2 = Sequential(
            Conv2d(32, 32, 3),
            ReLU(),
            MaxPool2d(2, 2)
        )
        self.fc1 = Linear(32 * 6 * 6, 100)
        self.fc2 = Linear(100, 4)

    def forward(self, x):
        B, T, C, H, W = x.shape
        x = x.view(B * T, C, H, W)
        out = self.layer1(x)
        out = self.layer2(out)
        out = out.view(out.size(0), -1)
        out = out.view(B, T, -1).mean(dim=1)
        out = self.fc1(out)
        out = self.fc2(out)
        return out